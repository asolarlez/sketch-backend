# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])
AC_INIT([SKETCH],[1.7.5],[asolar@csail.mit.edu])
AC_CONFIG_HEADERS([config.h])
LT_INIT([disable-shared static])
AM_INIT_AUTOMAKE
AC_PREFIX_DEFAULT(/usr)

# Checks for programs.
AC_PROG_CXX
if test "$CXX" = ":"; then
   AC_MSG_ERROR([You must install a C++ compiler, e.g. gcc4])
fi

AC_PROG_YACC
if test "x${YACC:0:5}" != "xbison"; then
   AC_MSG_ERROR([You must install Bison])
fi

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CPP
AC_PROG_LEX
LT_INIT
if test "x$LEX" != "xflex"; then
   AC_MSG_ERROR([You must install Flex])
fi

AC_PROG_MAKE_SET
#AC_PROG_RANLIB

AC_CONFIG_MACRO_DIR([m4])

# Checks for libraries
AC_SEARCH_LIBS([pthread_create], [pthread])
AC_SEARCH_LIBS([clock_gettime], [rt])
AC_SUBST([CEGIS_SO_VERSION], [1:2:0])
AC_SUBST([CEGIS_API_VERSION], [1.4.0])
# Checks for header files.
AC_FUNC_ALLOCA
AC_HEADER_DIRENT
m4_warn([obsolete],
[The preprocessor macro `STDC_HEADERS' is obsolete.
  Except in unusual embedded environments, you can safely include all
  ISO C90 headers unconditionally.])dnl
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are probably safe to remove.
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP

AC_CHECK_HEADERS([fcntl.h float.h inttypes.h libintl.h limits.h malloc.h stddef.h stdlib.h string.h strings.h sys/time.h sys/resource.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
m4_warn([obsolete],
[Update your code to rely only on HAVE_SYS_TIME_H,
then remove this warning and the obsolete code below it.
All current systems provide time.h; it need not be checked for.
Not all systems provide sys/time.h, but those that do, all allow
you to include it and time.h simultaneously.])dnl
AC_CHECK_HEADERS_ONCE([sys/time.h])
# Obsolete code to be removed.
if test $ac_cv_header_sys_time_h = yes; then
  AC_DEFINE([TIME_WITH_SYS_TIME],[1],[Define to 1 if you can safely include both <sys/time.h>
	     and <time.h>.  This macro is obsolete.])
fi
# End of obsolete code.

AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T
AC_C_VOLATILE

# Checks for library functions.
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_SETVBUF_REVERSED
m4_warn([obsolete],
[your code may safely assume C89 semantics that RETSIGTYPE is void.
Remove this warning and the `AC_CACHE_CHECK' when you adjust the code.])dnl
AC_CACHE_CHECK([return type of signal handlers],[ac_cv_type_signal],[AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM([#include <sys/types.h>
#include <signal.h>
],
		 [return *(signal (0, 0)) (0) == 1;])],
		   [ac_cv_type_signal=int],
		   [ac_cv_type_signal=void])])
AC_DEFINE_UNQUOTED([RETSIGTYPE],[$ac_cv_type_signal],[Define as the return type of signal handlers
		    (`int' or `void').])

AC_FUNC_STRCOLL
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([atexit floor getpagesize gettimeofday mblen memchr memmove memset pow putenv select sqrt strchr strcspn strdup strerror strpbrk strrchr strspn strstr strtol strtoul])

CFLAGS="-O3 -std=c++11 -include limits"
#CFLAGS="-O3 -std=c++0x"

if test "`ld --help | grep -o '\-\-stack'`" == "--stack"; then
LIBS=-Wl,--stack,16777216
fi

AC_CONFIG_FILES([Makefile 

    src/Makefile
    src/MiniSat/Makefile
    src/SketchSolver/Makefile
])
AC_OUTPUT
