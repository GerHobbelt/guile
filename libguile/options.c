/* Copyright 1995-1996,1998,2000-2001,2006,2008-2011,2018
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

#include "boolean.h"
#include "list.h"
#include "mallocs.h"
#include "numbers.h"
#include "pairs.h"
#include "strings.h"
#include "symbols.h"

#include "options.h"




/* {Run-time options}
 *
 * This is the basic interface for low-level configuration of the
 * Guile library.  It is used for configuring the reader, evaluator,
 * printer and debugger.
 *
 * Motivation:
 *
 * 1. Altering option settings can have side effects.
 * 2. Option values can be stored in native format.
 *    (Important for efficiency in, e. g., the evaluator.)
 * 3. Doesn't use up name space.
 * 4. Options can be naturally grouped => ease of use.
 */

/* scm_options is the core of all options interface procedures.
 *
 * Some definitions:
 *
 * Run time options in Guile are arranged in groups.  Each group
 * affects a certain aspect of the behavior of the library.
 *
 * An "options interface procedure" manages one group of options.  It
 * can be used to check or set options, or to get documentation for
 * all options of a group.  The options interface procedure is not
 * intended to be called directly by the user.  The user should
 * instead call
 *
 *   (<group>-options)
 *   (<group>-options 'help)
 *   (<group>-options 'full)
 *
 * to display current option settings (The second version also
 * displays documentation.  The third version also displays
 * information about programmer's options.), and
 *
 *   (<group>-enable  '<option-symbol>)
 *   (<group>-disable '<option-symbol>)
 *   (<group>-set! <option-symbol> <value>)
 *   (<group>-options <option setting>)
 *
 * to alter the state of an option  (The last version sets all
 * options according to <option setting>.) where <group> is the name
 * of the option group.
 *
 * An "option setting" represents the state of all low-level options
 * managed by one options interface procedure.  It is a list of
 * single symbols and symbols followed by a value.
 *
 * For boolean options, the presence of the symbol of that option in
 * the option setting indicates a true value.  If the symbol isn't a
 * member of the option setting this represents a false value.
 *
 * Other options are represented by a symbol followed by the value.
 *
 * If scm_options is called without arguments, the current option
 * setting is returned.  If the argument is an option setting, options
 * are altered and the old setting is returned.  If the argument isn't
 * a list, a list of sublists is returned, where each sublist contains
 * option name, value and documentation string.
 */

SCM_SYMBOL (scm_yes_sym, "yes");
SCM_SYMBOL (scm_no_sym, "no");

/* Return a list of the current option setting.  The format of an
 * option setting is described in the above documentation.  */
static SCM
get_option_setting (const scm_t_option options[])
{
  unsigned int i;
  SCM ls = SCM_EOL;
  for (i = 0; options[i].name; ++i)
    {
      switch (options[i].type)
	{
	case SCM_OPTION_BOOLEAN:
	  if (options[i].val)
	    ls = scm_cons (SCM_PACK (options[i].name), ls);
	  break;
	case SCM_OPTION_INTEGER:
	  ls = scm_cons (scm_from_unsigned_integer (options[i].val), ls);
	  ls = scm_cons (SCM_PACK (options[i].name), ls);
	  break;
	case SCM_OPTION_SCM:
	  ls = scm_cons (SCM_PACK (options[i].val), ls);
	  ls = scm_cons (SCM_PACK (options[i].name), ls);
	}
    }
  return ls;
}


/* Return a list of sublists, where each sublist contains option name, value
 * and documentation string.  */
static SCM
get_documented_option_setting (const scm_t_option options[])
{
  SCM ans = SCM_EOL;
  unsigned int i;

  for (i = 0; options[i].name; ++i)
    {
      SCM ls = scm_cons (scm_from_utf8_string (options[i].doc), SCM_EOL);
      switch (options[i].type)
	{
	case SCM_OPTION_BOOLEAN:
	  ls = scm_cons (options[i].val ? scm_yes_sym : scm_no_sym, ls);
	  break;
	case SCM_OPTION_INTEGER:
	  ls = scm_cons (scm_from_unsigned_integer (options[i].val), ls);
	  break;
	case SCM_OPTION_SCM:
	  ls = scm_cons (SCM_PACK (options[i].val), ls);
	}
      ls = scm_cons (SCM_PACK (options[i].name), ls);
      ans = scm_cons (ls, ans);
    }
  return scm_reverse_x (ans, SCM_UNDEFINED);
}


static int
options_length (scm_t_option options[])
{
  unsigned int i = 0;
  for (; options[i].name != NULL; ++i)
    ;

  return i;
}

/* Alters options according to the given option setting 'args'.  The value of
 * args is known to be a list, but it is not known whether the list is a well
 * formed option setting, i. e. if for every non-boolean option a value is
 * given.  For this reason, the function applies all changes to a copy of the
 * original setting in memory.  Only if 'args' was successfully processed,
 * the new setting will overwrite the old one.
 *
 * If DRY_RUN is set, don't change anything. This is useful for trying out an option
 * before entering a critical section.
 */
static void
change_option_setting (SCM args, scm_t_option options[], const char *s,
		       int dry_run)
{
  unsigned int i;
  scm_t_bits *new_vals;

  new_vals = scm_gc_malloc (options_length (options) * sizeof (scm_t_bits),
                            "new-options");

  for (i = 0; options[i].name; ++i)
    {
      if (options[i].type == SCM_OPTION_BOOLEAN)
	new_vals[i] = 0;
      else
	new_vals[i] = options[i].val;
    }

  while (!SCM_NULL_OR_NIL_P (args))
    {
      SCM name = SCM_CAR (args);
      int found = 0;

      for (i = 0; options[i].name && !found; ++i)
	{
	  if (scm_is_eq (name, SCM_PACK (options[i].name)))
	    {
	      switch (options[i].type)
		{
		case SCM_OPTION_BOOLEAN:
		  new_vals[i] = 1;
		  break;
		case SCM_OPTION_INTEGER:
		  args = SCM_CDR (args);
		  new_vals[i] = scm_to_size_t (scm_car (args));
		  break;
		case SCM_OPTION_SCM:
		  args = SCM_CDR (args);
		  new_vals[i] = SCM_UNPACK (scm_car (args));
		  break;
		}
	      found = 1;
	    }
	}

      if (!found)
	scm_misc_error (s, "Unknown option name: ~S", scm_list_1 (name));

      args = SCM_CDR (args);
    }

  if (dry_run)
    return;
  
  for (i = 0; options[i].name; ++i)
    options[i].val = new_vals[i];
}


SCM
scm_options (SCM args, scm_t_option options[], const char *s)
{
  return scm_options_try (args, options, s, 0);
}
	
SCM
scm_options_try (SCM args, scm_t_option options[], const char *s,
		 int dry_run)
{
  if (SCM_UNBNDP (args))
    return get_option_setting (options);
  else if (!SCM_NULL_OR_NIL_P (args) && !scm_is_pair (args))
    /* Dirk:FIXME:: This criterion should be improved.  IMO it is better to
     * demand that args is #t if documentation should be shown than to say
     * that every argument except a list will print out documentation.  */
    return get_documented_option_setting (options);
  else
    {
      SCM old_setting;
      SCM_ASSERT (scm_is_true (scm_list_p (args)), args, 1, s);
      old_setting = get_option_setting (options);
      change_option_setting (args, options, s, dry_run);
      return old_setting;
    }
}


void
scm_init_opts (SCM (*func) (SCM), scm_t_option options[])
{
  unsigned int i;

  for (i = 0; options[i].name; ++i)
    {
      SCM name = scm_from_utf8_symbol (options[i].name);
      options[i].name =	(char *) SCM_UNPACK (name);
    }
  func (SCM_UNDEFINED);
}


void
scm_init_options ()
{
#include "options.x"
}
