## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 1998-2004, 2006-2014, 2016-2024
##     Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

include $(top_srcdir)/am/snarf
include $(srcdir)/lightening/lightening.am

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@

# Override Automake's `DEFAULT_INCLUDES'.  By default, it contains
# "-I$(srcdir)", which causes problems on Tru64 where our "random.h"
# is picked up by <stdlib.h> instead of the libc's <random.h>.
DEFAULT_INCLUDES =

## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.  Also look for Gnulib headers in `lib'.
AM_CPPFLAGS = -DBUILDING_LIBGUILE=1 -I$(top_srcdir) -I$(top_builddir) \
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib -iquote$(builddir) \
	      $(LIBFFI_CFLAGS)

if ENABLE_JIT
AM_CPPFLAGS += -I$(top_srcdir)/libguile/lightening
endif

AM_CFLAGS = $(GCC_CFLAGS) $(CFLAG_VISIBILITY)

## The Gnulib Libtool archive.
gnulib_library = $(top_builddir)/lib/libgnu.la

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

lib_LTLIBRARIES = libguile-@GUILE_EFFECTIVE_VERSION@.la
bin_PROGRAMS = guile

noinst_PROGRAMS = guile_filter_doc_snarfage gen-scmconfig

gen_scmconfig_SOURCES = gen-scmconfig.c

## Override default rule; this should be compiled for BUILD host.  Note
## that we don't add $(AM_CPPFLAGS) here, as we need to run this
## program, but $(top_srcdir)/lib has a gnulib configured for the
## target.  Instead we manually add $(top_builddir) and $(builddir), in
## order to pick up the generated config.h and gen-scmconfig.h.  Nothing
## else from Guile is included by this code generator.
gen-scmconfig.$(OBJEXT): gen-scmconfig.c
	$(AM_V_GEN) \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) $(DEFS) $(DEFAULT_INCLUDES) -I$(top_builddir) \
		   -iquote$(builddir) -c -o $@ $<; \
	else \
		$(COMPILE) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
gen-scmconfig$(EXEEXT): $(gen_scmconfig_OBJECTS) $(gen_scmconfig_DEPENDENCIES)
	$(AM_V_GEN) \
	rm -f gen-scmconfig$(EXEEXT); \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(gen_scmconfig_OBJECTS); \
	else \
		$(LINK) $(gen_scmconfig_OBJECTS) $(LDADD) $(LIBS); \
	fi

scmconfig.h: ${top_builddir}/config.h gen-scmconfig$(EXEEXT)
	$(AM_V_GEN)(set -e; rm -f scmconfig.h.tmp; \
	cat $(srcdir)/scmconfig.h.top > scmconfig.h.tmp; \
	./gen-scmconfig$(EXEEXT) >> scmconfig.h.tmp; \
	chmod 444 scmconfig.h.tmp; \
	rm -f scmconfig.h; \
	mv scmconfig.h.tmp scmconfig.h)


guile_filter_doc_snarfage_SOURCES = c-tokenize.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
c-tokenize.$(OBJEXT): c-tokenize.c
	$(AM_V_GEN)							\
	if [ "$(cross_compiling)" = "yes" ]; then			\
		$(CC_FOR_BUILD) -DCROSS_COMPILING=1 -I$(top_builddir)	\
	           -c -o "$@" "$<";					\
	else								\
		$(COMPILE) -c -o "$@" "$<";				\
	fi

## Override default rule; this should run on BUILD host.
guile_filter_doc_snarfage$(EXEEXT): $(guile_filter_doc_snarfage_OBJECTS) $(guile_filter_doc_snarfage_DEPENDENCIES)
	$(AM_V_GEN) \
	rm -f guile_filter_doc_snarfage$(EXEEXT); \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(guile_filter_doc_snarfage_OBJECTS); \
	else \
		$(LINK) $(guile_filter_doc_snarfage_OBJECTS) $(LDADD) $(gnulib_library) $(LIBS); \
	fi


guile_SOURCES = guile.c
guile_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
guile_LDADD = libguile-@GUILE_EFFECTIVE_VERSION@.la
guile_LDFLAGS = $(GUILE_CFLAGS)

libguile_@GUILE_EFFECTIVE_VERSION@_la_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)

libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES =				\
	alist.c					\
	array-handle.c				\
	array-map.c				\
	arrays.c				\
	async.c					\
	atomic.c				\
	backtrace.c				\
	boolean.c				\
	bitvectors.c				\
	bytevectors.c				\
	chars.c					\
	chooks.c				\
	control.c				\
	continuations.c				\
	custom-ports.c				\
	debug.c					\
	deprecated.c				\
	deprecation.c				\
	dynstack.c				\
	dynwind.c				\
	eq.c					\
	error.c					\
	eval.c					\
	evalext.c				\
	exceptions.c				\
	expand.c				\
	extensions.c				\
	fdes-finalizers.c			\
	feature.c				\
	filesys.c				\
	finalizers.c				\
	fluids.c				\
	foreign.c				\
	foreign-object.c			\
	fports.c				\
	frames.c				\
	gc-malloc.c				\
	gc.c					\
	gettext.c				\
	generalized-vectors.c			\
	goops.c					\
	gsubr.c					\
	guardians.c				\
	hash.c					\
	hashtab.c				\
	hooks.c					\
	i18n.c					\
	init.c					\
	inline.c				\
	instructions.c				\
	integers.c				\
	intrinsics.c				\
	ioext.c					\
	jit.c					\
	keywords.c				\
	list.c					\
	load.c					\
	loader.c				\
	macros.c				\
	mallocs.c				\
	memoize.c				\
	modules.c				\
	null-threads.c				\
	numbers.c				\
	objprop.c				\
	options.c				\
	pairs.c					\
	poll.c					\
	ports.c					\
	print.c					\
	procprop.c				\
	procs.c					\
	programs.c				\
	promises.c				\
	r6rs-ports.c				\
	random.c				\
	rdelim.c				\
	read.c					\
	rw.c					\
	scmsigs.c				\
	script.c				\
	simpos.c				\
	smob.c					\
	sort.c					\
	srcprop.c				\
	srfi-4.c				\
	srfi-13.c				\
	srfi-14.c				\
	srfi-60.c				\
	stackchk.c				\
	stacks.c				\
	stime.c					\
	strings.c				\
	strorder.c				\
	strports.c				\
	struct.c				\
	symbols.c				\
	syntax.c				\
	threads.c				\
	throw.c					\
	unicode.c				\
	uniform.c				\
	values.c				\
	variable.c				\
	vectors.c				\
	version.c				\
	vm.c					\
	vports.c				\
	weak-set.c				\
	weak-table.c				\
	weak-vector.c

if ENABLE_JIT
libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES += $(lightening_c_files)
endif

if USE_MINI_GMP
libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES += mini-gmp.c
endif

DOT_X_FILES =					\
	alist.x					\
	array-handle.x				\
	array-map.x				\
	arrays.x				\
	async.x					\
	atomic.x				\
	backtrace.x				\
	boolean.x				\
	bitvectors.x				\
	bytevectors.x				\
	chars.x					\
	control.x				\
	continuations.x				\
	custom-ports.x				\
	debug.x					\
	deprecated.x				\
	deprecation.x				\
	dynl.x					\
	dynwind.x				\
	eq.x					\
	error.x					\
	eval.x					\
	evalext.x				\
	exceptions.x				\
	expand.x				\
	extensions.x				\
	fdes-finalizers.x			\
	feature.x				\
	filesys.x				\
	fluids.x				\
	foreign.x				\
	fports.x				\
	frames.x				\
	gc-malloc.x				\
	gc.x					\
	gettext.x				\
	generalized-vectors.x			\
	goops.x					\
	gsubr.x					\
	guardians.x				\
	hash.x					\
	hashtab.x				\
	hooks.x					\
	i18n.x					\
	init.x					\
	instructions.x				\
	intrinsics.x				\
	ioext.x					\
	keywords.x				\
	list.x					\
	load.x					\
	loader.x				\
	macros.x				\
	mallocs.x				\
	memoize.x				\
	modules.x				\
	numbers.x				\
	objprop.x				\
	options.x				\
	pairs.x					\
	ports.x					\
	print.x					\
	procprop.x				\
	procs.x					\
	programs.x				\
	promises.x				\
	r6rs-ports.x				\
	random.x				\
	rdelim.x				\
	read.x					\
	rw.x					\
	scmsigs.x				\
	script.x				\
	simpos.x				\
	smob.x					\
	sort.x					\
	srcprop.x				\
	srfi-4.x				\
	srfi-13.x				\
	srfi-14.x				\
	srfi-60.x				\
	stackchk.x				\
	stacks.x				\
	stime.x					\
	strings.x				\
	strorder.x				\
	strports.x				\
	struct.x				\
	symbols.x				\
	syntax.x				\
	threads.x				\
	throw.x					\
	unicode.x				\
	uniform.x				\
	values.x				\
	variable.x				\
	vectors.x				\
	version.x				\
	vm.x					\
	weak-set.x				\
	weak-table.x				\
	weak-vector.x

EXTRA_DOT_X_FILES = @EXTRA_DOT_X_FILES@

DOT_DOC_FILES = 				\
	alist.doc				\
	array-handle.doc			\
	array-map.doc				\
	arrays.doc				\
	async.doc				\
	atomic.doc				\
	backtrace.doc				\
	boolean.doc				\
	bitvectors.doc				\
	bytevectors.doc				\
	chars.doc				\
	control.doc				\
	continuations.doc			\
	custom-ports.doc			\
	debug.doc				\
	deprecated.doc				\
	deprecation.doc				\
	dynl.doc				\
	dynwind.doc				\
	eq.doc					\
	error.doc				\
	eval.doc				\
	evalext.doc				\
	exceptions.doc				\
	expand.doc				\
	extensions.doc				\
	fdes-finalizers.doc			\
	feature.doc				\
	filesys.doc 				\
	fluids.doc				\
	foreign.doc				\
	fports.doc				\
	gc-malloc.doc				\
	gc.doc					\
	gettext.doc				\
	generalized-vectors.doc			\
	goops.doc				\
	gsubr.doc				\
	guardians.doc				\
	hash.doc				\
	hashtab.doc				\
	hooks.doc				\
	i18n.doc				\
	init.doc				\
	ioext.doc				\
	keywords.doc				\
	list.doc				\
	load.doc				\
	macros.doc				\
	mallocs.doc				\
	memoize.doc				\
	modules.doc				\
	numbers.doc				\
	objprop.doc				\
	options.doc				\
	pairs.doc				\
	ports.doc				\
	print.doc				\
	procprop.doc				\
	procs.doc				\
	promises.doc				\
	r6rs-ports.doc				\
	random.doc				\
	rdelim.doc				\
	read.doc				\
	rw.doc					\
	scmsigs.doc				\
	script.doc				\
	simpos.doc				\
	smob.doc				\
	sort.doc				\
	srcprop.doc				\
	srfi-4.doc				\
	srfi-13.doc				\
	srfi-14.doc				\
	srfi-60.doc				\
	stackchk.doc				\
	stacks.doc				\
	stime.doc				\
	strings.doc				\
	strorder.doc				\
	strports.doc				\
	struct.doc				\
	symbols.doc				\
	syntax.doc				\
	threads.doc				\
	throw.doc				\
	unicode.doc				\
	uniform.doc				\
	values.doc				\
	variable.doc				\
	vectors.doc				\
	version.doc				\
	vports.doc				\
	weak-set.doc				\
	weak-table.doc				\
	weak-vector.doc

EXTRA_DOT_DOC_FILES = @EXTRA_DOT_DOC_FILES@

vm-operations.h: vm-engine.c
	@echo '/* This file was generated automatically from $<; do not' > $@
	@echo '   edit.  See the source file for copyright information.  */' >> $@
	@echo '' >> $@
	@echo "#define FOR_EACH_VM_OPERATION(M) \\" >> $@
	$(AM_V_GEN)$(GREP) '^ *VM_DEFINE_OP' $< \
	| sed -e 's,VM_DEFINE_OP (\(.*\)).*,  M (\1) \\,' >> $@
	@echo '' >> $@

srfi-14.i.c: $(srcdir)/UnicodeData.txt $(srcdir)/unidata_to_charset.awk
	$(AM_V_GEN)$(AWK) -f $(srcdir)/unidata_to_charset.awk < "$<" > "$@"

BUILT_INCLUDES = vm-operations.h scmconfig.h libpath.h srfi-14.i.c
BUILT_SOURCES = cpp-E.c cpp-SIG.c $(BUILT_INCLUDES) \
    $(DOT_X_FILES) $(EXTRA_DOT_X_FILES)

EXTRA_libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES = \
    syscalls.h					\
    dynl.c regex-posix.c			\
    posix.c net_db.c socket.c			\
    debug-malloc.c				\
    posix-w32.c					\
    locale-categories.h			\
    mini-gmp.h

## delete guile-snarf.awk from the installation bindir, in case it's
## lingering there due to an earlier guile version not having been
## wiped out.
install-exec-hook:
	rm -f $(DESTDIR)$(bindir)/guile-snarf.awk

## Instantiate a template.
INSTANTIATE =									\
  $(SED) -e 's,[@]pkgdatadir[@],$(pkgdatadir),g'				\
         -e 's,[@]pkglibdir[@],$(pkglibdir),g'					\
         -e 's,[@]GUILE_EFFECTIVE_VERSION[@],$(GUILE_EFFECTIVE_VERSION),g'

install-data-hook: libguile-@GUILE_EFFECTIVE_VERSION@-gdb.scm
	@$(MKDIR_P) $(DESTDIR)$(libdir)
## We want to install libguile-X.Y-gdb.scm as SOMETHING-gdb.scm.
## SOMETHING is the full name of the final library.  We want to ignore
## symlinks, the .la file, and any previous -gdb.py file.  This is
## inherently fragile, but there does not seem to be a better option,
## because libtool hides the real names from us.  (Trick courtesy of
## GNU libstdc++.)
	@here=`pwd`; cd $(DESTDIR)$(libdir);			\
	  for file in libguile-@GUILE_EFFECTIVE_VERSION@*; do	\
	    case $$file in					\
	      *-gdb.scm) ;;					\
	      *.la) ;;						\
	      *) if test -h $$file; then			\
	           continue;					\
	         fi;						\
	         libname=$$file;;				\
	    esac;						\
	  done;							\
	cd $$here;						\
	echo " $(INSTALL_DATA) $<				\
$(DESTDIR)$(libdir)/$$libname-gdb.scm";				\
	$(INSTANTIATE) "$<" > $$libname-gdb.scm;		\
	$(INSTALL_DATA) $$libname-gdb.scm			\
	    "$(DESTDIR)$(libdir)/$$libname-gdb.scm";		\
	rm $$libname-gdb.scm

# Remove the GDB support file and the Info 'dir' file that
# 'install-info' 5.x installs.
uninstall-hook:
	-rm "$(DESTDIR)$(libdir)/libguile-@GUILE_EFFECTIVE_VERSION@"*-gdb.scm
	-rm -f "$(DESTDIR)$(infodir)/dir"

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included.  So instead we list them here.
## Perhaps we can deal with them normally once the merge seems to be
## working.
noinst_HEADERS = custom-ports.h					\
                 elf.h						\
                 integers.h					\
                 intrinsics.h					\
                 quicksort.i.c                                  \
                 atomics-internal.h				\
                 cache-internal.h				\
                 posix-w32.h					\
		 private-options.h 				\
		 ports-internal.h 				\
		 syntax.h 					\
		 weak-list.h

# vm instructions
noinst_HEADERS += vm-engine.c

libguile_@GUILE_EFFECTIVE_VERSION@_la_DEPENDENCIES = @LIBLOBJS@

libguile_@GUILE_EFFECTIVE_VERSION@_la_LIBADD =		\
  @LIBLOBJS@ $(gnulib_library) $(LTLIBGMP) $(LTLIBLTDL)

# Libtool version info.
version_info = @LIBGUILE_INTERFACE_CURRENT@:@LIBGUILE_INTERFACE_REVISION@:@LIBGUILE_INTERFACE_AGE@

libguile_@GUILE_EFFECTIVE_VERSION@_la_LDFLAGS =	\
  $(BDW_GC_LIBS) $(LIBFFI_LIBS)			\
  $(CEIL_LIBM)					\
  $(FLOOR_LIBM)					\
  $(FREXP_LIBM)					\
  $(GETADDRINFO_LIB)				\
  $(HOSTENT_LIB)				\
  $(INET_NTOP_LIB)				\
  $(INET_PTON_LIB)				\
  $(ISNAND_LIBM)				\
  $(ISNANF_LIBM)				\
  $(ISNANL_LIBM)				\
  $(LDEXP_LIBM)					\
  $(LIBSOCKET)					\
  $(LIB_CLOCK_GETTIME)				\
  $(LOG1P_LIBM)					\
  $(LOG_LIBM)					\
  $(LTLIBICONV)					\
  $(LTLIBINTL)					\
  $(LTLIBUNISTRING)				\
  $(ROUND_LIBM)					\
  $(SERVENT_LIB)				\
  $(TRUNC_LIBM)					\
  -version-info $(version_info)			\
  -export-dynamic -no-undefined			\
  $(GNU_LD_FLAGS)

if HAVE_LD_VERSION_SCRIPT

libguile_@GUILE_EFFECTIVE_VERSION@_la_LDFLAGS += -Wl,--version-script="$(srcdir)/libguile.map"

endif HAVE_LD_VERSION_SCRIPT


# These are headers visible as <guile/mumble.h>
pkginclude_HEADERS =

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(pkgincludedir)/$(GUILE_EFFECTIVE_VERSION)/libguile
modinclude_HEADERS =				\
	scm.h					\
	__scm.h					\
	alist.h					\
	array-handle.h				\
	array-map.h				\
	arrays.h				\
	async.h					\
	atomic.h				\
	backtrace.h				\
	bdw-gc.h				\
	boolean.h				\
	bitvectors.h				\
	bytevectors.h				\
	chars.h					\
	chooks.h				\
	control.h				\
	continuations.h				\
	debug-malloc.h				\
	debug.h					\
	deprecated.h				\
	deprecation.h				\
	dynl.h					\
	dynstack.h				\
	dynwind.h				\
	eq.h					\
	error.h					\
	eval.h					\
	evalext.h				\
	exceptions.h				\
	expand.h				\
	extensions.h				\
	fdes-finalizers.h			\
	feature.h				\
	finalizers.h				\
	filesys.h				\
	fluids.h				\
	foreign.h				\
	foreign-object.h			\
	fports.h				\
	frames.h				\
	gc.h					\
	gc-inline.h				\
	gettext.h				\
	generalized-vectors.h			\
	goops.h					\
	gsubr.h					\
	guardians.h				\
	hash.h					\
	hashtab.h				\
	hooks.h					\
	i18n.h					\
	init.h					\
	inline.h				\
	instructions.h				\
	ioext.h					\
	iselect.h				\
	jit.h					\
	keywords.h				\
	list.h					\
	load.h					\
	loader.h				\
	macros.h				\
	mallocs.h				\
	memoize.h				\
	modules.h				\
	net_db.h				\
	null-threads.h				\
	numbers.h				\
	objprop.h				\
	options.h				\
	pairs.h					\
	poll.h					\
	ports.h					\
	posix.h					\
	print.h					\
	procprop.h				\
	procs.h					\
	programs.h				\
	promises.h				\
	pthread-threads.h			\
	r6rs-ports.h				\
	random.h				\
	rdelim.h				\
	read.h					\
	regex-posix.h				\
	rw.h					\
	scmsigs.h				\
	script.h				\
	setjump-win.h                           \
	simpos.h				\
	smob.h					\
	snarf.h					\
	socket.h				\
	sort.h					\
	srcprop.h				\
	srfi-4.h				\
	srfi-13.h				\
	srfi-14.h				\
	srfi-60.h				\
	stackchk.h				\
	stacks.h				\
	stime.h					\
	strings.h				\
	strorder.h				\
	strports.h				\
	struct.h				\
	symbols.h				\
	tags.h					\
	threads.h				\
	throw.h					\
	validate.h				\
	unicode.h				\
	uniform.h				\
	values.h				\
	variable.h				\
	vectors.h				\
	vm-builtins.h				\
	vm-expand.h				\
	vm.h					\
	vports.h				\
	weak-set.h				\
	weak-table.h				\
	weak-vector.h

nodist_modinclude_HEADERS = version.h scmconfig.h

bin_SCRIPTS = guile-snarf

# We can re-enable install for some of these if/when they are documented
# and people feel like maintaining them.  For now, this is not the case.
noinst_SCRIPTS = guile-snarf-docs

EXTRA_DIST = ChangeLog-scm ChangeLog-threads				\
    ChangeLog-1996-1999 ChangeLog-2000 ChangeLog-2008			\
    guile-func-name-check						\
    cpp-E.syms cpp-E.c cpp-SIG.syms cpp-SIG.c				\
    c-tokenize.lex							\
    scmconfig.h.top libgettext.h libguile.map				\
    unidata_to_charset.awk UnicodeData.txt				\
    vm-operations.h libguile-@GUILE_EFFECTIVE_VERSION@-gdb.scm		\
    $(lightening_c_files) $(lightening_extra_files)

## FIXME: Consider using timestamp file, to avoid unnecessary rebuilds.
if MINGW_LIBPATH
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "'`$(CYGPATH_W) $(pkgdatadir) | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "'`$(CYGPATH_W) $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION) | $(SED) 's/\\\\/\\\\\\\\/g'`'"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "'`$(CYGPATH_W) $(sitedir) | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_GLOBAL_SITE_DIR "'`$(CYGPATH_W) $(pkgdatadir)/site | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_LIB_DIR "'`$(CYGPATH_W) $(libdir) | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_EXTENSIONS_DIR "'`$(CYGPATH_W) $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_CCACHE_DIR "'`$(CYGPATH_W) $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/ccache | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_SITE_CCACHE_DIR "'`$(CYGPATH_W) $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/site-ccache | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_EFFECTIVE_VERSION "'`$(CYGPATH_W) $(GUILE_EFFECTIVE_VERSION) | $(SED) 's/\\\\/\\\\\\\\/g'`'"' >> libpath.tmp
	@echo '#define SCM_BUILD_INFO { \' >> libpath.tmp
	@echo '	{ "srcdir",        "'`cd @srcdir@; pwd | xargs $(CYGPATH_W) | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo ' { "top_srcdir",    "'`$(CYGPATH_W) @top_srcdir_absolute@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "prefix",	   "'`$(CYGPATH_W) @prefix@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "exec_prefix",   "'`$(CYGPATH_W) @exec_prefix@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "bindir",	   "'`$(CYGPATH_W) @bindir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "sbindir",	   "'`$(CYGPATH_W) @sbindir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "libexecdir",	   "'`$(CYGPATH_W) @libexecdir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "datadir",	   "'`$(CYGPATH_W) @datadir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "sysconfdir",	   "'`$(CYGPATH_W) @sysconfdir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "sharedstatedir", "'`$(CYGPATH_W) @sharedstatedir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "localstatedir", "'`$(CYGPATH_W) @localstatedir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "libdir",	   "'`$(CYGPATH_W) @libdir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "ccachedir",	   SCM_CCACHE_DIR }, \' >> libpath.tmp
	@echo '	{ "infodir",	   "'`$(CYGPATH_W) @infodir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "mandir",	   "'`$(CYGPATH_W) @mandir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "includedir",	   "'`$(CYGPATH_W) @includedir@ | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "pkgdatadir",	   "'`$(CYGPATH_W) $(pkgdatadir)  | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "'`$(CYGPATH_W) $(pkglibdir)  | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "'`$(CYGPATH_W) $(pkgincludedir)  | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' \
		>> libpath.tmp
	@echo '	{ "extensiondir", "'`$(CYGPATH_W) $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions | $(SED) 's/\\\\/\\\\\\\\/g'`'" }, \' >> libpath.tmp
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileinterface", "@LIBGUILE_INTERFACE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '	{ "CFLAGS",	   "@GUILE_CFLAGS@" }, \' >> libpath.tmp
	@BUILD_DATE="$${SOURCE_DATE_EPOCH:-`date '+%s'`}" ; \
	 echo '	{ "buildstamp",    "'`date -u -d @$$BUILD_DATE +'%Y-%m-%d %T'`'" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	$(AM_V_GEN)mv libpath.tmp libpath.h
else
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "$(pkgdatadir)"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "$(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "$(sitedir)"' >> libpath.tmp
	@echo '#define SCM_GLOBAL_SITE_DIR "$(pkgdatadir)/site"' >> libpath.tmp
	@echo '#define SCM_LIB_DIR "$(libdir)"' >> libpath.tmp
	@echo '#define SCM_EXTENSIONS_DIR "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions"' >> libpath.tmp
	@echo '#define SCM_CCACHE_DIR "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/ccache"' >> libpath.tmp
	@echo '#define SCM_SITE_CCACHE_DIR "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/site-ccache"' >> libpath.tmp
	@echo '#define SCM_BUILD_INFO { \' >> libpath.tmp
	@echo '	{ "srcdir", "'"`cd @srcdir@; pwd`"'" }, \' >> libpath.tmp
	@echo ' { "top_srcdir",    "@top_srcdir_absolute@" }, \' >> libpath.tmp
	@echo '	{ "prefix",	   "@prefix@" }, \' >> libpath.tmp
	@echo '	{ "exec_prefix",   "@exec_prefix@" }, \' >> libpath.tmp
	@echo '	{ "bindir",	   "@bindir@" }, \' >> libpath.tmp
	@echo '	{ "sbindir",	   "@sbindir@" }, \' >> libpath.tmp
	@echo '	{ "libexecdir",	   "@libexecdir@" }, \' >> libpath.tmp
	@echo '	{ "datadir",	   "@datadir@" }, \' >> libpath.tmp
	@echo '	{ "sysconfdir",	   "@sysconfdir@" }, \' >> libpath.tmp
	@echo '	{ "sharedstatedir", "@sharedstatedir@" }, \' >> libpath.tmp
	@echo '	{ "localstatedir", "@localstatedir@" }, \' >> libpath.tmp
	@echo '	{ "libdir",	   "@libdir@" }, \' >> libpath.tmp
	@echo '	{ "ccachedir",	   SCM_CCACHE_DIR }, \' >> libpath.tmp
	@echo '	{ "infodir",	   "@infodir@" }, \' >> libpath.tmp
	@echo '	{ "mandir",	   "@mandir@" }, \' >> libpath.tmp
	@echo '	{ "includedir",	   "@includedir@" }, \' >> libpath.tmp
	@echo '	{ "pkgdatadir",	   "$(pkgdatadir)" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "$(pkglibdir)" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "$(pkgincludedir)" }, \' \
		>> libpath.tmp
	@echo '	{ "extensiondir", "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions" }, \' >> libpath.tmp
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileinterface", "@LIBGUILE_INTERFACE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '	{ "CFLAGS",	   "@GUILE_CFLAGS@" }, \' >> libpath.tmp
	@BUILD_DATE="$${SOURCE_DATE_EPOCH:-`date '+%s'`}" ; \
	 echo '	{ "buildstamp",    "'`date -u -d @$$BUILD_DATE +'%Y-%m-%d %T'`'" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	$(AM_V_GEN)mv libpath.tmp libpath.h
endif

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x .doc

.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)
.c.doc:
	$(AM_V_SNARF)./guile-snarf-docs -o $@ $< -- $(snarfcppopts)

$(DOT_X_FILES) $(EXTRA_DOT_X_FILES): $(BUILT_INCLUDES) snarf.h guile-snarf.in version.h

$(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES): $(BUILT_INCLUDES) snarf.h guile-snarf-docs.in guile_filter_doc_snarfage$(EXEEXT)

error.x: cpp-E.c
posix.x: cpp-SIG.c

alldotdocfiles    = $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES)
snarf2checkedtexi = GUILE_AUTO_COMPILE=0 $(top_builddir)/meta/build-env guild snarf-check-and-output-texi
dotdoc2texi       = cat $(alldotdocfiles) | $(snarf2checkedtexi)

guile.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi) --manual > $@ || { rm $@; false; }

guile-procedures.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(AM_V_GEN)$(dotdoc2texi)          > $@ || { rm $@; false; }

# Requires meta to be listed before libguile in SUBDIRS
guile-procedures.txt: guile-procedures.texi
	$(AM_V_GEN)						\
	$(top_builddir)/meta/guile --no-auto-compile		\
	  "$(srcdir)/texi-fragments-to-docstrings"		\
	  "$(builddir)/guile-procedures.texi"			\
	  > $@.tmp
	@mv $@.tmp $@

c-tokenize.c: c-tokenize.lex
	flex -t $(srcdir)/c-tokenize.lex > $@ || { rm $@; false; }

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)


# This page is for maintenance of the lists of CPP symbols that are eventually
# included in error.c (‘errno’ values: E*) and posix.c (signal names: SIG*),
# in the funcs scm_init_{error,posix}, respectively.
#
# The lists (files cpp-{E,SIG}.syms) are not included verbatim, but processed
# (via pattern rule ‘.syms.c’ below) so that each CPP symbol is transformed to
# fragment of #ifdef'd C code that defines a Scheme variable of the same name.
# The resulting files, cpp-{E,SIG}.c, are the ones #include:d.
#
# To maintain the (source) lists of symbols, we use a GCC-specific feature
# to list all symbols #define:d when expanding <{errno,signal}.h>, grep
# the output for {E,SIG}*, massage the result into a usable form, and
# construct a union of the current and new symbols.  This is written to
# files cpp-{E,SIG}.syms.NEW for review.  If things look ok, you can then
# "mv foo.NEW foo" and commit.  Generating the foo.NEW files and showing
# an informative message is the job of targets chknew-{E,SIG}.

# For each line in foo.syms, transform:
#	SYMBOL
# to:
#	#ifdef SYMBOL
#	scm_c_define ("SYMBOL", scm_from_int (SYMBOL));
#	#endif
# writing output to foo.c.  We use a while-read loop instead of a
# one-line sed script because some seds (e.g., FreeBSD) don't read
# '\n' as a newline (like GNU sed does).  Sad, but true.
.syms.c:
	$(AM_V_GEN) 							\
	cat $< | while read sym ; do					\
	  echo "#ifdef $$sym" ;						\
	  echo "scm_c_define (\"$$sym\", scm_from_int ($$sym));" ;	\
	  echo "#endif" ;						\
	done > $@T
	@mv $@T $@

# Write $(srcdir)/cpp-{E,SIG}.syms.NEW if there are any not-yet-seen
# ("new" to us) E* or SIG* symbols in <errno.h> or <signal.h>, respectively.
chknew-E chknew-SIG:
	@bit=`echo $@ | sed s/^chknew-//` ;			\
	old="$(srcdir)/cpp-$$bit.syms" ;			\
	echo "#include <$${bit}.h>"				\
	 | sed 's/E/errno/;s/SIG/signal/'			\
	 | gcc -dM -E -						\
	 | sed 's/^#define //;/^'$$bit'[0-9A-Z][0-9A-Z]* /!d;s/ .*//'	\
	 | sort | diff -u $$old - | sed '1,2d;/^+/!d;s/^.//'	\
	 > TMP ;						\
	if [ -s TMP ] ; then new="$$old.NEW" ;			\
	  cat $$old TMP | sort > $$new ;			\
	  echo "$$new: `sed -n '$$=' TMP` new symbol(s)." ;	\
	  sed 's/^/  /' TMP ;					\
	else echo "No new symbols found." ;			\
	fi ; rm TMP



MOSTLYCLEANFILES = \
	scmconfig.h scmconfig.h.tmp

CLEANFILES = libpath.h *.x *.doc srfi-14.i.c guile-procedures.txt	\
     guile-procedures.texi guile.texi

MAINTAINERCLEANFILES = c-tokenize.c
