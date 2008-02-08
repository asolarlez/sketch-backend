## To regenerate these includes, run 'gen_makefile_includes.sh'
ABC = .
include modules.sources.mk
include modules.includes.mk

noinst_LIBRARIES = libabc.a
libabc_a_CFLAGS = $(ABC_INCS)
libabc_a_SOURCES = $(ABC_SRCS)
