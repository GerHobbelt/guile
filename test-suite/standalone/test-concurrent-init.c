/* Copyright 2025
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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <assert.h>
#include <pthread.h>

static void *
guile_main (void *arg)
{
  scm_display (scm_cons (scm_from_latin1_symbol ("hello"),
                         scm_from_uintptr_t ((uintptr_t) arg)),
               scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  return arg;
}

static void *
thread_main (void *arg)
{
  return scm_with_guile (guile_main, arg);
}

int
main (int argc, char *argv[])
{
  pthread_t t[3];

  for (int i = 0; i < 3; i++)
    pthread_create (&t[i], NULL, thread_main, t + i);

  for (int i = 0; i < 3; i++)
    {
      void *result;
      pthread_join (t[i], &result);
      assert (result == t + i);
    }

  return EXIT_SUCCESS;
}
