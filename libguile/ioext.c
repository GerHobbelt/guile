/* Copyright 1995-2001,2003,2006,2011,2014,2018
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
#  include <config.h>
#endif

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#ifdef HAVE_IO_H
#include <io.h>
#endif

#include "async.h"
#include "dynwind.h"
#include "extensions.h"
#include "fdes-finalizers.h"
#include "feature.h"
#include "fports.h"
#include "gsubr.h"
#include "hashtab.h"
#include "numbers.h"
#include "pairs.h"
#include "ports-internal.h"
#include "ports.h"
#include "strings.h"
#include "syscalls.h"
#include "weak-set.h"
#include "version.h"

#include "ioext.h"




SCM_DEFINE (scm_ftell, "ftell", 1, 0, 0, 
            (SCM fd_port),
	    "Return an integer representing the current position of\n"
	    "@var{fd_port}, measured from the beginning.  Equivalent to:\n"
	    "\n"
	    "@lisp\n"
	    "(seek port 0 SEEK_CUR)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_ftell
{
  return scm_seek (fd_port, SCM_INUM0, scm_from_int (SEEK_CUR));
}
#undef FUNC_NAME

SCM_DEFINE (scm_redirect_port, "redirect-port", 2, 0, 0,
            (SCM old, SCM new),
	    "This procedure takes two ports and duplicates the underlying file\n"
	    "descriptor from @var{old} into @var{new}.  The\n"
	    "current file descriptor in @var{new} will be closed.\n"
	    "After the redirection the two ports will share a file position\n"
	    "and file status flags.\n\n"
	    "The return value is unspecified.\n\n"
	    "Unexpected behavior can result if both ports are subsequently used\n"
	    "and the original and/or duplicate ports are buffered.\n\n"
	    "This procedure does not have any side effects on other ports or\n"
	    "revealed counts.")
#define FUNC_NAME s_scm_redirect_port
{
  int ans, oldfd, newfd;
  scm_t_fport *fp;

  old = SCM_COERCE_OUTPORT (old);
  new = SCM_COERCE_OUTPORT (new);
  
  SCM_VALIDATE_OPFPORT (1, old);
  SCM_VALIDATE_OPFPORT (2, new);
  oldfd = SCM_FPORT_FDES (old);
  fp = SCM_FSTREAM (new);
  newfd = fp->fdes;
  if (oldfd != newfd)
    {
      /* Ensure there is nothing in either port's input or output
         buffers.  */
      if (SCM_OUTPUT_PORT_P (old))
        scm_flush (old);
      if (SCM_INPUT_PORT_P (old) && SCM_PORT (old)->rw_random)
        scm_end_input (old);

      if (SCM_OUTPUT_PORT_P (new))
        scm_flush (new);
      if (SCM_INPUT_PORT_P (new) && SCM_PORT (new)->rw_random)
        scm_end_input (new);

      ans = dup2 (oldfd, newfd);
      if (ans == -1)
	SCM_SYSERROR;

      SCM_PORT (new)->rw_random = SCM_PORT (old)->rw_random;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_dup_to_fdes, "dup->fdes", 1, 1, 0, 
            (SCM fd_or_port, SCM fd),
	    "Return a new integer file descriptor referring to the open file\n"
	    "designated by @var{fd_or_port}, which must be either an open\n"
	    "file port or a file descriptor.")
#define FUNC_NAME s_scm_dup_to_fdes
{
  int oldfd, newfd, rv;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  if (scm_is_integer (fd_or_port))
    oldfd = scm_to_int (fd_or_port);
  else
    {
      SCM_VALIDATE_OPFPORT (1, fd_or_port);
      oldfd = SCM_FPORT_FDES (fd_or_port);
    }

  if (SCM_UNBNDP (fd))
    {
      newfd = dup (oldfd);
      if (newfd == -1)
	SCM_SYSERROR;
      fd = scm_from_int (newfd);
    }
  else
    {
      newfd = scm_to_int (fd);
      if (oldfd != newfd)
	{
	  scm_evict_ports (newfd);	/* see scsh manual.  */
	  rv = dup2 (oldfd, newfd);
	  if (rv == -1)
	    SCM_SYSERROR;
	}
    }
  return fd;
}
#undef FUNC_NAME


SCM_DEFINE (scm_dup2, "dup2", 2, 0, 0, 
            (SCM oldfd, SCM newfd),
	    "A simple wrapper for the @code{dup2} system call.\n"
	    "Copies the file descriptor @var{oldfd} to descriptor\n"
	    "number @var{newfd}, replacing the previous meaning\n"
	    "of @var{newfd}.  Both @var{oldfd} and @var{newfd} must\n"
	    "be integers.\n"
	    "Unlike for dup->fdes or primitive-move->fdes, no attempt\n"
	    "is made to move away ports which are using @var{newfd}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_dup2
{
  int c_oldfd;
  int c_newfd;
  int rv;

  c_oldfd = scm_to_int (oldfd);
  c_newfd = scm_to_int (newfd);
  rv = dup2 (c_oldfd, c_newfd);
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fileno, "fileno", 1, 0, 0, 
            (SCM port),
	    "Return the integer file descriptor underlying @var{port}.  Does\n"
	    "not change its revealed count.")
#define FUNC_NAME s_scm_fileno
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPFPORT (1, port);
  return scm_from_int (SCM_FPORT_FDES (port));
}
#undef FUNC_NAME

/* GJB:FIXME:: why does this not throw
   an error if the arg is not a port?
   This proc as is would be better names isattyport?
   if it is not going to assume that the arg is a port

   [cmm] I don't see any problem with the above.  why should a type
   predicate assume _anything_ about its argument?
*/
SCM_DEFINE (scm_isatty_p, "isatty?", 1, 0, 0, 
            (SCM port),
	    "Return @code{#t} if @var{port} is using a serial non--file\n"
	    "device, otherwise @code{#f}.")
#define FUNC_NAME s_scm_isatty_p
{
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  if (!SCM_OPFPORTP (port))
    return SCM_BOOL_F;
  
  rv = isatty (SCM_FPORT_FDES (port));
  return  scm_from_bool(rv);
}
#undef FUNC_NAME



SCM_DEFINE (scm_fdopen, "fdopen", 2, 0, 0,
            (SCM fdes, SCM modes),
	    "Return a new port based on the file descriptor @var{fdes}.\n"
	    "Modes are given by the string @var{modes}.  The revealed count\n"
	    "of the port is initialized to zero.  The modes string is the\n"
	    "same as that accepted by @ref{File Ports, open-file}.")
#define FUNC_NAME s_scm_fdopen
{
  return scm_i_fdes_to_port (scm_to_int (fdes),
			     scm_i_mode_bits (modes), SCM_BOOL_F,
                             SCM_FPORT_OPTION_VERIFY);
}
#undef FUNC_NAME



/* Move a port's underlying file descriptor to a given value.
 * Returns  #f if fdes is already the given value.
 *          #t if fdes moved. 
 * MOVE->FDES is implemented in Scheme and calls this primitive.
 */
SCM_DEFINE (scm_primitive_move_to_fdes, "primitive-move->fdes", 2, 0, 0,
            (SCM port, SCM fd),
	    "Moves the underlying file descriptor for @var{port} to the integer\n"
	    "value @var{fd} without changing the revealed count of @var{port}.\n"
	    "Any other ports already using this descriptor will be automatically\n"
	    "shifted to new descriptors and their revealed counts reset to zero.\n"
	    "The return value is @code{#f} if the file descriptor already had the\n"
	    "required value or @code{#t} if it was moved.")
#define FUNC_NAME s_scm_primitive_move_to_fdes
{
  scm_t_fport *stream;
  int old_fd;
  int new_fd;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1, port);
  stream = SCM_FSTREAM (port);
  old_fd = stream->fdes;
  new_fd = scm_to_int (fd);
  if  (old_fd == new_fd)
    {
      return SCM_BOOL_F;
    }
  scm_evict_ports (new_fd);
  rv = dup2 (old_fd, new_fd);
  if (rv == -1)
    SCM_SYSERROR;
  stream->fdes = new_fd;
  scm_run_fdes_finalizers (old_fd);
  SCM_SYSCALL (close (old_fd));  
  return SCM_BOOL_T;
}
#undef FUNC_NAME

static SCM
get_matching_port (void *closure, SCM port, SCM result)
{
  int fd = * (int *) closure;
  
  if (SCM_OPFPORTP (port) && SCM_FSTREAM (port)->fdes == fd)
    result = scm_cons (port, result);

  return result;
}

/* Return a list of ports using a given file descriptor.  */
SCM_DEFINE (scm_fdes_to_ports, "fdes->ports", 1, 0, 0, 
           (SCM fd),
	    "Return a list of existing ports which have @var{fd} as an\n"
	    "underlying file descriptor, without changing their revealed\n"
	    "counts.")
#define FUNC_NAME s_scm_fdes_to_ports
{
  SCM result = SCM_EOL;
  int int_fd = scm_to_int (fd);

  result = scm_c_weak_set_fold (get_matching_port,
                                (void*) &int_fd, result, 
                                scm_i_port_weak_set);
  return result;
}
#undef FUNC_NAME    


static void
scm_init_ice_9_ioext (void)
{
#include "ioext.x"
}

void 
scm_init_ioext ()
{
  scm_add_feature ("i/o-extensions");

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_ice_9_ioext",
			    (scm_t_extension_init_func) scm_init_ice_9_ioext,
			    NULL);
}

