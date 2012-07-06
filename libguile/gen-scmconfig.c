
/**********************************************************************

  Description of Guile's public config header mechanics:
  ----------------------------------------------------- 

  Guile has four core headers:

    config.h: Guile's private automatically generated configuration
      header -- generated by configure.in and autoheader.  *NOT*
      installed during "make install" and so may not be referred to by
      any public headers.

    libguile/_scm.h: Guile's private core header.  _scm.h is not
      installed.  It's only visible to the libguile sources
      themselves, and it includes config.h, the private config header.
      Among other things this file provides a place to make decisions
      based on the information gathered in config.h.

    libguile/scmconfig.h: Guile's public automatically generated
      configuration header -- generated at build time by concatenating
      the contents of libguile/scmconfig.h.top with the output from
      libguile/gen-scmconfig.  gen-scmconfig bases its output on the
      information in the private config.h header, the contents of
      gen-scmconfig.h (which is created by configure.in from
      gen-scmconfig.h.in), and the information provided in this file,
      gen-scmconfig.c.

    libguile/__scm.h: Guile's public core header.  This file is
      installed and publically visible.  It includes
      libguile/scmconfig.h, the public config header and provides a
      place to make decisions based on the information gathered in
      scmconfig.h to define things that other headers can depend on.

   Notes and guidelines:

   - use 1 and 0 for public #defines instead of "def and undef",
     i.e. use #define SCM_HAVE_FOO rather than just not defining
     SCM_HAVE_FOO whenever possible.  See GNU Coding Guidelines for
     rationale.  The only notable non-deprecated exceptions to this
     rule are GUILE_DEBUG and GUILE_DEBUG_FREELIST which do not follow
     this convention in order to retain backward compatibility.

   - in the code below, be *VERY* careful not to use or rely on any
     runtime-dynamic information below.  For example, you cannot use
     sizeof (FOO), but must use static information like SIZEOF_BAR
     (from config.h) or SCM_SIZEOF_BAZ (from scmconfig.h).  This is
     because the gcc that is compiling gen-scmconfig.c and/or the
     machine that is running gen-scmconfig may not be the same
     compiler and/or hardware that will eventually be running Guile.
     (i.e. keep the cross-compilation case in mind).

   - try to avoid adding names to the public namespace when possible.
     Note in the code below, that in a number of cases, we detect a
     feature and based on that, we decide whether or not to print
     anything at all.  This decreases the extraneous #defines and
     #ifdefery that we require in scmconfig.h

   - try to avoid adding any duplicate definitions to config.h and
     scmconfig.h.  i.e. have just SCM_ENABLE_ELISP in scmconfig.h
     rather than ENABLE_ELISP in config.h and SCM_ENABLE_ELISP in
     scmconfig.h.

   - in cases where you need to communicate information from
     configure.in to gen-scmconfig.c, don't add an AC_DEFINE unless
     you need it for other purposes.  Just add a suitable SCM_I_GSC_*
     variable to configure.in, set the value, AC_SUBST the value, and
     add an appropriate line to gen-scmconfig.h.in.  All gen-scmconfig
     related AC_SUBST vars should be prefixed with SCM_I_GSC_.

   - make sure that anything that we explicitly typedef publically is
     prefixed with scm_t_.  i.e. we used to typedef long to ptrdiff_t
     if we didn't detect ptrdiff_t, but this has been changed so that
     we typedef scm_t_ptrdiff instead so that we won't conflict with
     any non-guile header definitions of the same type.  For types
     like intptr_t and uintptr_t which we just try to detect and don't
     actually define, it's fine not to have a corresponding scm_t_
     type.

   - we now use SCM_SIZEOF_FOO != 0 rather than SCM_HAVE_FOO for any
     cases where the size might actually vary.

   Rationales (not finished):

     Why do we use a C program here rather than AC_OUTPUT_COMMANDS?
     --------------------------------------------------------------

     The main reason is that there are some values we would need
     access to at AC_OUTPUT_COMMANDs that are determined by configure
     but are not available at AC_OUTPUT time.  The values are *only*
     available via config.h.  We use gen-scmconfig so we can see those
     values and make decisions based on their settings.

     Why have gen-scmconfig.h.in?
     ----------------------------

     Without that header, we could end up needing multiple aliases for
     public settings like SCM_ENABLE_ELISP.  We can't define
     SCM_ENABLE_ELISP in config.h since that header is private and any
     definition in scmconfig.h would conflict (#ifndef might be
     possible but runs the risk of conflicting directives), so a
     likely solution would be to AC_DEFINE([SCM_I_ENABLE_ELISP]), and
     then use SCM_I_ENABLE_ELISP in gen-scmconfig via config.h to
     determine whether or not to #define SCM_ENABLE_ELISP, but this
     leaves us with two #defined symbols for each public setting --
     better to just have one value (public or private) that all code
     uses.

     Having this header means we can AC_SUBST a value like
     SCM_I_GSC_ENABLE_ELISP and then set it in here via AC_OUTPUT
     substitutions, and gen-scmconfig can use that definition to
     determine whether or not to #define SCM_ENABLE_ELISP when
     generating scmconfig.h, and we end up with nothing extraneous
     added to config.h.

 **********************************************************************/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <libguile/gen-scmconfig.h>

#include <stdio.h>
#include <string.h>

#define pf printf

int
main (int argc, char *argv[])
{
  pf ("/* This file is automatically generated --"
      " see configure.in for details */\n"
      "\n"
      "#ifndef SCM_SCMCONFIG_H\n"
      "#define SCM_SCMCONFIG_H\n");
  
  /*** various important headers ***/
  pf ("\n");
  pf ("/* Important headers */\n");
  if (SCM_I_GSC_NEEDS_STDINT_H)
    pf ("#include <stdint.h>\n");
  if (SCM_I_GSC_NEEDS_INTTYPES_H)
    pf ("#include <inttypes.h>\n");

#ifdef HAVE_LIMITS_H
  pf ("#include <limits.h>\n");
#else
  pf ("/* limits.h not available */\n");
#endif

#if HAVE_SYS_TIME_H
  pf ("#include <sys/time.h>\n");
#else
  pf ("/* sys/time.h not available */\n");
#endif

#if HAVE_TIME_H
  pf ("#include <time.h>\n");
#else
  pf ("/* time.h not available */\n");
#endif

  pf("\n");
#ifdef STDC_HEADERS
  pf ("#define SCM_HAVE_STDC_HEADERS 1 /* 0 or 1 */\n");
  pf ("#include <stdlib.h>\n");
# ifdef HAVE_SYS_TYPES_H
  pf ("#include <sys/types.h>\n");
# endif
# ifdef HAVE_SYS_STDTYPES_H
  pf ("#include <sys/stdtypes.h>\n");
# endif
  pf ("#include <stddef.h>\n");
#else /* STDC_HEADERS */
  pf ("#define SCM_HAVE_STDC_HEADERS 0 /* 0 or 1 */");
#endif /* def STDC_HEADERS */

  pf("\n");
#ifdef HAVE_SYS_SELECT_H
  pf ("#define SCM_HAVE_SYS_SELECT_H 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_SYS_SELECT_H 0 /* 0 or 1 */\n");
#endif

#ifdef HAVE_WINSOCK2_H
  pf ("#define SCM_HAVE_WINSOCK2_H 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_WINSOCK2_H 0 /* 0 or 1 */\n");
#endif


  /*** GUILE_DEBUG (defined or undefined) ***/
  pf ("\n");
  pf ("/* Define to include various undocumented debugging functions. */\n");
  if (SCM_I_GSC_GUILE_DEBUG)
    pf ("#define GUILE_DEBUG 1 /* defined or undefined */\n");
  else
    pf ("/* #undef GUILE_DEBUG */\n");

  /*** SCM_ENABLE_DEPRECATED (0 or 1) ***/
  pf ("\n");
  pf ("/* Set to 1 if you want to enable deprecated features. */\n");
  pf ("/* (value will be 0 or 1). */\n");
  pf ("#define SCM_ENABLE_DEPRECATED %d\n", SCM_I_GSC_ENABLE_DEPRECATED);

  /*** SCM_STACK_GROWS_UP (0 or 1) ***/
  pf ("\n");
  pf ("/* Set to 1 if the stack grows up, 0 otherwise. */\n");
  pf ("#define SCM_STACK_GROWS_UP %d /* 0 or 1 */\n",
      SCM_I_GSC_STACK_GROWS_UP);

  /*** SCM_C_INLINE (defined to appropriate string or undefined) ***/
  pf ("\n");
  pf ("/* C compiler's syntax for inline functions if any,\n"
      "   otherwise undefined. */\n");
  if (SCM_I_GSC_C_INLINE)
    pf ("#define SCM_C_INLINE %s\n", SCM_I_GSC_C_INLINE);
  else
    pf ("/* #undef SCM_C_INLINE */\n");

  pf ("\n");
  pf ("/* Standard types. */\n");

  pf ("/* These are always defined */\n");  
  pf ("#define SCM_SIZEOF_CHAR %d\n", SIZEOF_CHAR);
  pf ("#define SCM_SIZEOF_UNSIGNED_CHAR %d\n", SIZEOF_UNSIGNED_CHAR);
  pf ("#define SCM_SIZEOF_SHORT %d\n", SIZEOF_SHORT);
  pf ("#define SCM_SIZEOF_UNSIGNED_SHORT %d\n", SIZEOF_UNSIGNED_SHORT);
  pf ("#define SCM_SIZEOF_LONG %d\n", SIZEOF_LONG);
  pf ("#define SCM_SIZEOF_UNSIGNED_LONG %d\n", SIZEOF_UNSIGNED_LONG);
  pf ("#define SCM_SIZEOF_INT %d\n", SIZEOF_INT);
  pf ("#define SCM_SIZEOF_UNSIGNED_INT %d\n", SIZEOF_UNSIGNED_INT);
  pf ("#define SCM_SIZEOF_SIZE_T %d\n", SIZEOF_SIZE_T);

  pf ("\n");
  pf ("/* Size of (unsigned) long long or 0 if not available (scm_t_*64 may\n"
      "   be more likely to be what you want */\n");
  pf ("#define SCM_SIZEOF_LONG_LONG %d\n", SIZEOF_LONG_LONG);
  pf ("#define SCM_SIZEOF_UNSIGNED_LONG_LONG %d\n", SIZEOF_UNSIGNED_LONG_LONG);

  pf ("\n");
  pf ("/* These are always defined. */\n");
  pf ("typedef %s scm_t_int8;\n", SCM_I_GSC_T_INT8);
  pf ("typedef %s scm_t_uint8;\n", SCM_I_GSC_T_UINT8);
  pf ("typedef %s scm_t_int16;\n", SCM_I_GSC_T_INT16);
  pf ("typedef %s scm_t_uint16;\n", SCM_I_GSC_T_UINT16);
  pf ("typedef %s scm_t_int32;\n", SCM_I_GSC_T_INT32);
  pf ("typedef %s scm_t_uint32;\n", SCM_I_GSC_T_UINT32);
  pf ("typedef %s scm_t_intmax;\n", SCM_I_GSC_T_INTMAX);
  pf ("typedef %s scm_t_uintmax;\n", SCM_I_GSC_T_UINTMAX);
  pf ("typedef %s scm_t_intptr;\n", SCM_I_GSC_T_INTPTR);
  pf ("typedef %s scm_t_uintptr;\n", SCM_I_GSC_T_UINTPTR);

  if (0 == strcmp ("intmax_t", SCM_I_GSC_T_INTMAX))
    pf ("#define SCM_SIZEOF_INTMAX %d\n", SIZEOF_INTMAX_T);
  else if (0 == strcmp ("long long", SCM_I_GSC_T_INTMAX))
    pf ("#define SCM_SIZEOF_INTMAX %d\n", SIZEOF_LONG_LONG);
  else if (0 == strcmp ("__int64", SCM_I_GSC_T_INTMAX))
    pf ("#define SCM_SIZEOF_INTMAX %d\n", SIZEOF___INT64);
  else
    return 1;

  pf ("\n");
  pf ("#define SCM_HAVE_T_INT64 1 /* 0 or 1 */\n");
  pf ("typedef %s scm_t_int64;\n", SCM_I_GSC_T_INT64);
  pf ("#define SCM_HAVE_T_UINT64 1 /* 0 or 1 */\n");
  pf ("typedef %s scm_t_uint64;\n", SCM_I_GSC_T_UINT64);

  pf ("\n");
  pf ("/* scm_t_ptrdiff_t and size, always defined -- defined to long if\n"
      "   platform doesn't have ptrdiff_t. */\n");
  pf ("typedef %s scm_t_ptrdiff;\n", SCM_I_GSC_T_PTRDIFF);
  if (0 == strcmp ("long", SCM_I_GSC_T_PTRDIFF))
    pf ("#define SCM_SIZEOF_SCM_T_PTRDIFF %d\n", SIZEOF_LONG);
  else
    pf ("#define SCM_SIZEOF_SCM_T_PTRDIFF %d\n", SIZEOF_PTRDIFF_T);

  pf ("\n");
  pf ("/* Size of intptr_t or 0 if not available */\n");
  pf ("#define SCM_SIZEOF_INTPTR_T %d\n", SIZEOF_INTPTR_T);
  pf ("/* Size of uintptr_t or 0 if not available */\n");
  pf ("#define SCM_SIZEOF_UINTPTR_T %d\n", SIZEOF_UINTPTR_T);

  pf ("\n");
  pf ("/* same as POSIX \"struct timespec\" -- always defined */\n");
#ifdef HAVE_STRUCT_TIMESPEC
  pf ("typedef struct timespec scm_t_timespec;\n");
#else
  pf ("/* POSIX.4 structure for a time value.  This is like a `struct timeval'"
      "   but has nanoseconds instead of microseconds.  */\n");
  pf ("typedef struct\n"
      "{\n"
      "  long int tv_sec;		/* Seconds.  */\n"
      "  long int tv_nsec;		/* Nanoseconds.  */\n"
      "} scm_t_timespec;\n");
#endif

  pf ("\n");
  pf ("/*** Threading model (scmconfig.h support not finished) ***/\n");

  pf ("/* Define to 1 if using pthread multithreading. */\n");
  pf ("#define SCM_USE_PTHREAD_THREADS %d /* 0 or 1 */\n",
      SCM_I_GSC_USE_PTHREAD_THREADS);

  pf ("/* Define to 1 if using one-thread 'multi'threading. */\n");
  pf ("#define SCM_USE_NULL_THREADS %d /* 0 or 1 */\n",
      SCM_I_GSC_USE_NULL_THREADS);

  pf ("/* Define to 1 if need braces around PTHREAD_ONCE_INIT (for Solaris). */\n");
  pf ("#define SCM_NEED_BRACES_ON_PTHREAD_ONCE_INIT %d /* 0 or 1 */\n",
      SCM_I_GSC_NEED_BRACES_ON_PTHREAD_ONCE_INIT);

  pf ("/* Define to 1 if need braces around PTHREAD_MUTEX_INITIALIZER\n"
      "   (for IRIX with GCC)  */\n");
  pf ("#define SCM_NEED_BRACES_ON_PTHREAD_MUTEX_INITIALIZER %d /* 0 or 1 */\n",
      SCM_I_GSC_NEED_BRACES_ON_PTHREAD_MUTEX_INITIALIZER);

#ifdef HAVE_GC_PTHREAD_CANCEL
  pf ("#define SCM_HAVE_GC_PTHREAD_CANCEL 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_GC_PTHREAD_CANCEL 0 /* 0 or 1 */\n");
#endif

#ifdef HAVE_GC_PTHREAD_EXIT
  pf ("#define SCM_HAVE_GC_PTHREAD_EXIT 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_GC_PTHREAD_EXIT 0 /* 0 or 1 */\n");
#endif

#ifdef HAVE_GC_PTHREAD_SIGMASK
  pf ("#define SCM_HAVE_GC_PTHREAD_SIGMASK 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_GC_PTHREAD_SIGMASK 0 /* 0 or 1 */\n");
#endif

  pf ("\n\n/*** File system access ***/\n");

  pf ("/* Define to 1 if `struct dirent64' is available.  */\n");
  pf ("#define SCM_HAVE_STRUCT_DIRENT64 %d /* 0 or 1 */\n",
      SCM_I_GSC_HAVE_STRUCT_DIRENT64);

  pf ("/* Define to 1 if `readdir64_r ()' is available.  */\n");
#ifdef HAVE_READDIR64_R
  pf ("#define SCM_HAVE_READDIR64_R 1 /* 0 or 1 */\n");
#else
  pf ("#define SCM_HAVE_READDIR64_R 0 /* 0 or 1 */\n");
#endif

  /* Arrange so that we have a file offset type that reflects the one
     used when compiling Guile, regardless of what the application's
     `_FILE_OFFSET_BITS' says.  See
     http://lists.gnu.org/archive/html/bug-guile/2009-06/msg00018.html
     for the original bug report.

     Note that we can't define `scm_t_off' in terms of `off_t' or
     `off64_t' because they may or may not be available depending on
     how the application that uses Guile is compiled.  */

#if defined GUILE_USE_64_CALLS && defined HAVE_STAT64
  pf ("typedef scm_t_int64 scm_t_off;\n");
#elif SIZEOF_OFF_T == SIZEOF_INT
  pf ("typedef int scm_t_off;\n");
#else
  pf ("typedef long int scm_t_off;\n");
#endif

  pf ("/* Define to 1 if the compiler supports the "
      "`__thread' storage class.  */\n");
  if (SCM_I_GSC_HAVE_THREAD_STORAGE_CLASS)
    pf ("#define SCM_HAVE_THREAD_STORAGE_CLASS\n");
  else
    pf ("/* #undef SCM_HAVE_THREAD_STORAGE_CLASS */\n");

#ifdef USE_DLL_IMPORT
  pf ("\n");
  pf ("/* Define some additional CPP macros on Win32 platforms. */\n");
  pf ("# define __REGEX_IMPORT__ 1\n");
  pf ("# define __CRYPT_IMPORT__ 1\n");
  pf ("# define __READLINE_IMPORT__ 1\n");
  pf ("# define QT_IMPORT 1\n");
#endif

  pf ("\n");
  pf ("#if SCM_ENABLE_DEPRECATED == 1\n"
      "# define USE_THREADS 1 /* always true now */\n"
      "# define GUILE_ISELECT 1 /* always true now */\n"
      "# define READER_EXTENSIONS 1 /* always true now */\n"
      "# define DEBUG_EXTENSIONS 1 /* always true now */\n"
      "# define DYNAMIC_LINKING 1 /* always true now */\n"
      "#endif\n");
  printf ("\n");

  pf ("#define SCM_HAVE_ARRAYS 1 /* always true now */\n");

  pf ("\n");
  pf ("/* Constants from uniconv.h.  */\n");
  pf ("#define SCM_ICONVEH_ERROR %d\n", SCM_I_GSC_ICONVEH_ERROR);
  pf ("#define SCM_ICONVEH_QUESTION_MARK %d\n",
      SCM_I_GSC_ICONVEH_QUESTION_MARK);
  pf ("#define SCM_ICONVEH_ESCAPE_SEQUENCE %d\n",
      SCM_I_GSC_ICONVEH_ESCAPE_SEQUENCE);  

  printf ("#endif\n");

  return 0;
}
