/* Copyright 2012-2013,2018
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>

#ifndef _WIN64
#include <setjmp.h>
#else
#include "setjump-win.h"
#endif

#include "control.h"
#include "eval.h"
#include "fluids.h"
#include "variable.h"
#include "threads.h"

#include "dynstack.h"




#define PROMPT_WORDS 6
#define PROMPT_KEY(top) (SCM_PACK ((top)[0]))
#define PROMPT_FP(top) ((ptrdiff_t) ((top)[1]))
#define SET_PROMPT_FP(top, fp) do { top[1] = (scm_t_bits)(fp); } while (0)
#define PROMPT_SP(top) ((ptrdiff_t) ((top)[2]))
#define SET_PROMPT_SP(top, sp) do { top[2] = (scm_t_bits)(sp); } while (0)
#define PROMPT_VRA(top) ((uint32_t *) ((top)[3]))
#define PROMPT_MRA(top) ((uint8_t *) ((top)[4]))
#define PROMPT_JMPBUF(top) ((jmp_buf *) ((top)[5]))

#define WINDER_WORDS 2
#define WINDER_PROC(top) ((scm_t_guard) ((top)[0]))
#define WINDER_DATA(top) ((void *) ((top)[1]))

#define DYNWIND_WORDS 2
#define DYNWIND_ENTER(top) (SCM_PACK ((top)[0]))
#define DYNWIND_LEAVE(top) (SCM_PACK ((top)[1]))

#define WITH_FLUID_WORDS 2
#define WITH_FLUID_FLUID(top) (SCM_PACK ((top)[0]))
#define WITH_FLUID_VALUE_BOX(top) (SCM_PACK ((top)[1]))

#define DYNAMIC_STATE_WORDS 1
#define DYNAMIC_STATE_STATE_BOX(top) (SCM_PACK ((top)[0]))




static void
copy_scm_t_bits (scm_t_bits *dst, scm_t_bits *src, size_t n)
{
  size_t i;

  for (i = 0; i < n; i++)
    dst[i] = src[i];
}

static void
clear_scm_t_bits (scm_t_bits *items, size_t n)
{
  size_t i;

  for (i = 0; i < n; i++)
    items[i] = 0;
}

/* Ensure space for N additional words.  */
static void
dynstack_ensure_space (scm_t_dynstack *dynstack, size_t n)
{
  size_t capacity = SCM_DYNSTACK_CAPACITY (dynstack);
  size_t height = SCM_DYNSTACK_HEIGHT (dynstack);

  n += SCM_DYNSTACK_HEADER_LEN;

  if (capacity < height + n)
    {
      scm_t_bits *new_base;

      while (capacity < height + n)
        capacity = (capacity < 4) ? 8 : (capacity * 2);

      new_base = scm_gc_malloc (capacity * sizeof(scm_t_bits), "dynstack");

      copy_scm_t_bits (new_base, dynstack->base, height);
      clear_scm_t_bits (dynstack->base, height);
        
      dynstack->base = new_base;
      dynstack->top = new_base + height;
      dynstack->limit = new_base + capacity;
    }
}

static inline scm_t_bits *
push_dynstack_entry_unchecked (scm_t_dynstack *dynstack,
                               scm_t_dynstack_item_type type,
                               scm_t_bits flags, size_t len)
{
  scm_t_bits *ret = dynstack->top;

  SCM_DYNSTACK_SET_TAG (dynstack->top, SCM_MAKE_DYNSTACK_TAG (type, flags, len));
  dynstack->top += SCM_DYNSTACK_HEADER_LEN + len;
  SCM_DYNSTACK_SET_TAG (dynstack->top, 0);
  SCM_DYNSTACK_SET_PREV_OFFSET (dynstack->top, SCM_DYNSTACK_HEADER_LEN + len);

  return ret;
}

static inline scm_t_bits *
push_dynstack_entry (scm_t_dynstack *dynstack,
                     scm_t_dynstack_item_type type,
                     scm_t_bits flags, size_t len)
{
  if (SCM_UNLIKELY (!SCM_DYNSTACK_HAS_SPACE (dynstack, len)))
    dynstack_ensure_space (dynstack, len);
  return push_dynstack_entry_unchecked (dynstack, type, flags, len);
}
  
void
scm_dynstack_push_frame (scm_t_dynstack *dynstack,
                         scm_t_dynstack_frame_flags flags)
{
  push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_FRAME, flags, 0);
}

void
scm_dynstack_push_rewinder (scm_t_dynstack *dynstack,
                            scm_t_dynstack_winder_flags flags,
                            scm_t_guard proc, void *data)
{
  scm_t_bits *words;

  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_REWINDER, flags,
                               WINDER_WORDS);
  words[0] = (scm_t_bits) proc;
  words[1] = (scm_t_bits) data;
}

void
scm_dynstack_push_unwinder (scm_t_dynstack *dynstack,
                            scm_t_dynstack_winder_flags flags,
                            scm_t_guard proc, void *data)
{
  scm_t_bits *words;

  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_UNWINDER, flags,
                               WINDER_WORDS);
  words[0] = (scm_t_bits) proc;
  words[1] = (scm_t_bits) data;
}

/* The fluid is stored on the stack, but the value has to be stored on the heap,
   so that all continuations that capture this dynamic scope capture the same
   binding.  */
void
scm_dynstack_push_fluid (scm_t_dynstack *dynstack, SCM fluid, SCM value,
                         scm_t_dynamic_state *dynamic_state)
{
  scm_t_bits *words;
  SCM value_box;

  if (SCM_UNLIKELY (!SCM_FLUID_P (fluid)))
    scm_wrong_type_arg ("with-fluid*", 0, fluid);

  value_box = scm_make_variable (value);

  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_WITH_FLUID, 0,
                               WITH_FLUID_WORDS);
  words[0] = SCM_UNPACK (fluid);
  words[1] = SCM_UNPACK (value_box);

  /* Go ahead and swap them.  */
  scm_swap_fluid (fluid, value_box, dynamic_state);
}

void
scm_dynstack_push_prompt (scm_t_dynstack *dynstack,
                          scm_t_dynstack_prompt_flags flags,
                          SCM key,
                          ptrdiff_t fp_offset, ptrdiff_t sp_offset,
                          uint32_t *vra, uint8_t *mra, jmp_buf *registers)
{
  scm_t_bits *words;

  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_PROMPT, flags,
                               PROMPT_WORDS);
  words[0] = SCM_UNPACK (key);
  words[1] = (scm_t_bits) fp_offset;
  words[2] = (scm_t_bits) sp_offset;
  words[3] = (scm_t_bits) vra;
  words[4] = (scm_t_bits) mra;
  words[5] = (scm_t_bits) registers;
}

void
scm_dynstack_push_dynwind (scm_t_dynstack *dynstack, SCM enter, SCM leave)
{
  scm_t_bits *words;

  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_DYNWIND, 0,
                               DYNWIND_WORDS);
  words[0] = SCM_UNPACK (enter);
  words[1] = SCM_UNPACK (leave);
}

static inline scm_t_bits
dynstack_pop (scm_t_dynstack *dynstack, scm_t_bits **words)
{
  scm_t_bits *prev = SCM_DYNSTACK_PREV (dynstack->top);
  scm_t_bits tag;

  if (SCM_UNLIKELY (!prev))
    abort ();

  SCM_DYNSTACK_SET_PREV_OFFSET (dynstack->top, 0);
  dynstack->top = prev;

  tag = SCM_DYNSTACK_TAG (dynstack->top);
  SCM_DYNSTACK_SET_TAG (dynstack->top, 0);
  *words = dynstack->top;

  return tag;
}
  
void
scm_dynstack_push_dynamic_state (scm_t_dynstack *dynstack, SCM state,
                                 scm_t_dynamic_state *dynamic_state)
{
  scm_t_bits *words;
  SCM state_box;

  if (SCM_UNLIKELY (scm_is_false (scm_dynamic_state_p (state))))
    scm_wrong_type_arg ("with-dynamic-state", 0, state);

  state_box = scm_make_variable (scm_set_current_dynamic_state (state));
  words = push_dynstack_entry (dynstack, SCM_DYNSTACK_TYPE_DYNAMIC_STATE, 0,
                               DYNAMIC_STATE_WORDS);
  words[0] = SCM_UNPACK (state_box);
}

void
scm_dynstack_pop (scm_t_dynstack *dynstack)
{
  scm_t_bits tag, *words;
  tag = dynstack_pop (dynstack, &words);
  clear_scm_t_bits (words, SCM_DYNSTACK_TAG_LEN (tag));
}
  
scm_t_dynstack *
scm_dynstack_capture_all (scm_t_dynstack *dynstack)
{
  return scm_dynstack_capture (dynstack, SCM_DYNSTACK_FIRST (dynstack));
}

scm_t_dynstack *
scm_dynstack_capture (scm_t_dynstack *dynstack, scm_t_bits *item)
{
  char *mem;
  scm_t_dynstack *ret;
  size_t len;

  assert (item >= SCM_DYNSTACK_FIRST (dynstack));
  assert (item <= dynstack->top);

  len = dynstack->top - item + SCM_DYNSTACK_HEADER_LEN;
  mem = scm_gc_malloc (sizeof (*ret) + len * sizeof(scm_t_bits), "dynstack");
  ret = (scm_t_dynstack *) mem;
  ret->base = (scm_t_bits *) (mem + sizeof (*ret));
  ret->limit = ret->base + len;
  ret->top = ret->base + len;

  copy_scm_t_bits (ret->base, item - SCM_DYNSTACK_HEADER_LEN, len);
  SCM_DYNSTACK_SET_PREV_OFFSET (SCM_DYNSTACK_FIRST (ret), 0);

  return ret;
}

void
scm_dynstack_relocate_prompts (scm_t_dynstack *dynstack, ptrdiff_t base)
{
  scm_t_bits *walk;

  /* Relocate prompts.  */
  for (walk = dynstack->top; walk; walk = SCM_DYNSTACK_PREV (walk))
    {
      scm_t_bits tag = SCM_DYNSTACK_TAG (walk);

      if (SCM_DYNSTACK_TAG_TYPE (tag) == SCM_DYNSTACK_TYPE_PROMPT)
        {
          SET_PROMPT_FP (walk, PROMPT_FP (walk) - base);
          SET_PROMPT_SP (walk, PROMPT_SP (walk) - base);
        }
    }
}

void
scm_dynstack_wind_1 (scm_t_dynstack *dynstack, scm_t_bits *item)
{
  scm_t_bits tag = SCM_DYNSTACK_TAG (item);
  scm_t_dynstack_item_type type = SCM_DYNSTACK_TAG_TYPE (tag);
  scm_t_bits flags = SCM_DYNSTACK_TAG_FLAGS (tag);
  size_t len = SCM_DYNSTACK_TAG_LEN (tag);
  
  switch (type)
    {
    case SCM_DYNSTACK_TYPE_FRAME:
      if (!(flags & SCM_F_DYNSTACK_FRAME_REWINDABLE))
        scm_misc_error ("scm_dynstack_wind_1",
                        "cannot invoke continuation from this context",
                        SCM_EOL);
      break;

    case SCM_DYNSTACK_TYPE_UNWINDER:
      break;

    case SCM_DYNSTACK_TYPE_REWINDER:
      WINDER_PROC (item) (WINDER_DATA (item));
      break;

    case SCM_DYNSTACK_TYPE_WITH_FLUID:
      scm_swap_fluid (WITH_FLUID_FLUID (item),
                      WITH_FLUID_VALUE_BOX (item),
                      SCM_I_CURRENT_THREAD->dynamic_state);
      break;

    case SCM_DYNSTACK_TYPE_PROMPT:
      /* see vm_reinstate_partial_continuation */
      break;

    case SCM_DYNSTACK_TYPE_DYNWIND:
      scm_call_0 (DYNWIND_ENTER (item));
      break;

    case SCM_DYNSTACK_TYPE_DYNAMIC_STATE:
      scm_variable_set_x (DYNAMIC_STATE_STATE_BOX (item),
                          scm_set_current_dynamic_state
                          (scm_variable_ref (DYNAMIC_STATE_STATE_BOX (item))));
      break;

    case SCM_DYNSTACK_TYPE_NONE:
    default:
      abort ();
    }

  {
    scm_t_bits *words = push_dynstack_entry (dynstack, type, flags, len);

    copy_scm_t_bits (words, item, len);
  }
}

scm_t_bits
scm_dynstack_unwind_1 (scm_t_dynstack *dynstack)
{
  scm_t_bits tag;
  scm_t_bits *words;
  scm_t_dynstack_item_type type;

  tag = dynstack_pop (dynstack, &words);
  
  type = SCM_DYNSTACK_TAG_TYPE (tag);
  
  switch (type)
    {
    case SCM_DYNSTACK_TYPE_FRAME:
      break;

    case SCM_DYNSTACK_TYPE_UNWINDER:
      WINDER_PROC (words) (WINDER_DATA (words));
      clear_scm_t_bits (words, WINDER_WORDS);
      break;

    case SCM_DYNSTACK_TYPE_REWINDER:
      clear_scm_t_bits (words, WINDER_WORDS);
      break;

    case SCM_DYNSTACK_TYPE_WITH_FLUID:
      scm_swap_fluid (WITH_FLUID_FLUID (words),
                      WITH_FLUID_VALUE_BOX (words),
                      SCM_I_CURRENT_THREAD->dynamic_state);
      clear_scm_t_bits (words, WITH_FLUID_WORDS);
      break;

    case SCM_DYNSTACK_TYPE_PROMPT:
      /* we could invalidate the prompt */
      clear_scm_t_bits (words, PROMPT_WORDS);
      break;

    case SCM_DYNSTACK_TYPE_DYNWIND:
      {
        SCM proc = DYNWIND_LEAVE (words);
        clear_scm_t_bits (words, DYNWIND_WORDS);
        scm_call_0 (proc);
      }
      break;

    case SCM_DYNSTACK_TYPE_DYNAMIC_STATE:
      scm_variable_set_x (DYNAMIC_STATE_STATE_BOX (words),
                          scm_set_current_dynamic_state
                          (scm_variable_ref (DYNAMIC_STATE_STATE_BOX (words))));
      clear_scm_t_bits (words, DYNAMIC_STATE_WORDS);
      break;

    case SCM_DYNSTACK_TYPE_NONE:
    default:
      abort ();
    }

  return tag;
}

void
scm_dynstack_wind (scm_t_dynstack *dynstack, scm_t_bits *item)
{
  for (; SCM_DYNSTACK_TAG (item); item = SCM_DYNSTACK_NEXT (item))
    scm_dynstack_wind_1 (dynstack, item);
}

void
scm_dynstack_unwind (scm_t_dynstack *dynstack, scm_t_bits *base)
{
  while (dynstack->top > base)
    scm_dynstack_unwind_1 (dynstack);
}

static int
same_entries (scm_t_bits *walk_a, scm_t_bits *next_a,
              scm_t_bits *walk_b, scm_t_bits *next_b)
{
  if (SCM_DYNSTACK_TAG (walk_a) != SCM_DYNSTACK_TAG (walk_b))
    return 0;

  if (next_a - walk_a != next_b - walk_b)
    return 0;

  assert (SCM_DYNSTACK_PREV_OFFSET (next_a) == next_a - walk_a);
  assert (SCM_DYNSTACK_PREV_OFFSET (next_b) == next_b - walk_b);

  while (walk_a != next_a)
    if (*(walk_a++) != *(walk_b++))
      return 0;

  return 1;
}

static ptrdiff_t
shared_prefix_length (scm_t_dynstack *a, scm_t_dynstack *b)
{
  scm_t_bits *walk_a, *next_a, *walk_b, *next_b;

  walk_a = SCM_DYNSTACK_FIRST (a);
  walk_b = SCM_DYNSTACK_FIRST (b);

  next_a = SCM_DYNSTACK_NEXT (walk_a);
  next_b = SCM_DYNSTACK_NEXT (walk_b);

  while (next_a && next_b && same_entries (walk_a, next_a, walk_b, next_b))
    {
      walk_a = next_a;
      walk_b = next_b;

      next_a = SCM_DYNSTACK_NEXT (walk_a);
      next_b = SCM_DYNSTACK_NEXT (walk_b);
    }

  return walk_a - a->base;
}

scm_t_bits *
scm_dynstack_unwind_fork (scm_t_dynstack *dynstack, scm_t_dynstack *branch)
{
  ptrdiff_t join_height;

  join_height = shared_prefix_length (dynstack, branch);

  scm_dynstack_unwind (dynstack, dynstack->base + join_height);

  return branch->base + join_height;
}

scm_t_bits*
scm_dynstack_find_prompt (scm_t_dynstack *dynstack, SCM key,
                          scm_t_dynstack_prompt_flags *flags,
                          ptrdiff_t *fp_offset, ptrdiff_t *sp_offset,
                          uint32_t **vra, uint8_t **mra, jmp_buf **registers)
{
  scm_t_bits *walk;

  for (walk = SCM_DYNSTACK_PREV (dynstack->top); walk;
       walk = SCM_DYNSTACK_PREV (walk))
    {
      scm_t_bits tag = SCM_DYNSTACK_TAG (walk);

      if (SCM_DYNSTACK_TAG_TYPE (tag) == SCM_DYNSTACK_TYPE_PROMPT
          && scm_is_eq (PROMPT_KEY (walk), key))
        {
          if (flags)
            *flags = SCM_DYNSTACK_TAG_FLAGS (tag);
          if (fp_offset)
            *fp_offset = PROMPT_FP (walk);
          if (sp_offset)
            *sp_offset = PROMPT_SP (walk);
          if (vra)
            *vra = PROMPT_VRA (walk);
          if (mra)
            *mra = PROMPT_MRA (walk);
          if (registers)
            *registers = PROMPT_JMPBUF (walk);
          return walk;
        }
    }

  return NULL;
}

SCM
scm_dynstack_find_old_fluid_value (scm_t_dynstack *dynstack, SCM fluid,
                                   size_t depth, SCM dflt)
{
  scm_t_bits *walk;

  for (walk = SCM_DYNSTACK_PREV (dynstack->top); walk;
       walk = SCM_DYNSTACK_PREV (walk))
    {
      scm_t_bits tag = SCM_DYNSTACK_TAG (walk);

      switch (SCM_DYNSTACK_TAG_TYPE (tag))
        {
        case SCM_DYNSTACK_TYPE_WITH_FLUID:
          {
            if (scm_is_eq (WITH_FLUID_FLUID (walk), fluid))
              {
                if (depth == 0)
                  return SCM_VARIABLE_REF (WITH_FLUID_VALUE_BOX (walk));
                else
                  depth--;
              }
            break;
          }
        case SCM_DYNSTACK_TYPE_DYNAMIC_STATE:
          {
            SCM state, val;

            /* The previous dynamic state may or may not have
               established a binding for this fluid.  */
            state = scm_variable_ref (DYNAMIC_STATE_STATE_BOX (walk));
            val = scm_dynamic_state_ref (state, fluid, SCM_UNDEFINED);
            if (!SCM_UNBNDP (val))
              {
                if (depth == 0)
                  return val;
                else
                  depth--;
              }
            break;
          }
        default:
          break;
        }
    }

  return dflt;
}

void
scm_dynstack_wind_prompt (scm_t_dynstack *dynstack, scm_t_bits *item,
                          ptrdiff_t base_fp_offset,
                          jmp_buf *registers)
{
  scm_t_bits tag = SCM_DYNSTACK_TAG (item);

  if (SCM_DYNSTACK_TAG_TYPE (tag) != SCM_DYNSTACK_TYPE_PROMPT)
    abort ();

  scm_dynstack_push_prompt (dynstack,
                            SCM_DYNSTACK_TAG_FLAGS (tag),
                            PROMPT_KEY (item),
                            PROMPT_FP (item) + base_fp_offset,
                            PROMPT_SP (item) + base_fp_offset,
                            PROMPT_VRA (item),
                            PROMPT_MRA (item),
                            registers);
}

void
scm_dynstack_unwind_frame (scm_t_dynstack *dynstack)
{
  /* Unwind up to and including the next frame entry.  */
  while (1)
    {
      scm_t_bits tag, *words;

      tag = dynstack_pop (dynstack, &words);

      switch (SCM_DYNSTACK_TAG_TYPE (tag))
        {
        case SCM_DYNSTACK_TYPE_FRAME:
          return;
        case SCM_DYNSTACK_TYPE_REWINDER:
          clear_scm_t_bits (words, WINDER_WORDS);
          continue;
        case SCM_DYNSTACK_TYPE_UNWINDER:
          {
            scm_t_guard proc = WINDER_PROC (words);
            void *data = WINDER_DATA (words);
            clear_scm_t_bits (words, WINDER_WORDS);
            if (SCM_DYNSTACK_TAG_FLAGS (tag) & SCM_F_DYNSTACK_WINDER_EXPLICIT)
              proc (data);
            continue;
          }
        default:
          /* We should only see winders.  */
          abort ();
        }
    }
}

/* This function must not allocate.  */
void
scm_dynstack_unwind_fluid (scm_t_dynstack *dynstack,
                           scm_t_dynamic_state *dynamic_state)
{
  scm_t_bits tag, *words;
  size_t len;
  
  tag = dynstack_pop (dynstack, &words);
  len = SCM_DYNSTACK_TAG_LEN (tag);

  assert (SCM_DYNSTACK_TAG_TYPE (tag) == SCM_DYNSTACK_TYPE_WITH_FLUID);
  assert (len == WITH_FLUID_WORDS);

  scm_swap_fluid (WITH_FLUID_FLUID (words), WITH_FLUID_VALUE_BOX (words),
                  dynamic_state);
  clear_scm_t_bits (words, len);
}

void
scm_dynstack_unwind_dynamic_state (scm_t_dynstack *dynstack,
                                   scm_t_dynamic_state *dynamic_state)
{
  scm_t_bits tag, *words;
  size_t len;

  tag = dynstack_pop (dynstack, &words);
  len = SCM_DYNSTACK_TAG_LEN (tag);

  assert (SCM_DYNSTACK_TAG_TYPE (tag) == SCM_DYNSTACK_TYPE_DYNAMIC_STATE);
  assert (len == DYNAMIC_STATE_WORDS);

  scm_variable_set_x (DYNAMIC_STATE_STATE_BOX (words),
                      scm_set_current_dynamic_state
                      (scm_variable_ref (DYNAMIC_STATE_STATE_BOX (words))));
  clear_scm_t_bits (words, len);
}

