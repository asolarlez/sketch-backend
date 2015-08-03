# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(SKETCH, 1.4.0, arnold@cs.berkeley.edu)
AC_CONFIG_HEADER([config.h])
LT_INIT([shared static])
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
AC_PROG_LIBTOOL
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
AC_HEADER_STDC
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
AC_HEADER_TIME
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
AC_TYPE_SIGNAL
AC_FUNC_STRCOLL
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([atexit floor getpagesize gettimeofday mblen memchr memmove memset pow putenv select sqrt strchr strcspn strdup strerror strpbrk strrchr strspn strstr strtol strtoul])

CFLAGS=-O3

if test "`ld --help | grep -o '\-\-stack'`" == "--stack"; then
LIBS=-Wl,--stack,16777216
fi

AC_OUTPUT(
    Makefile 

    src/Makefile
    src/MiniSat/Makefile
    src/SketchSolver/Makefile
)
