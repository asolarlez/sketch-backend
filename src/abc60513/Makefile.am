## To regenerate these includes, run 'gen_makefile_includes.sh'
ABC = .
include modules.sources.mk
include modules.includes.mk
include modules.headers.mk

noinst_LTLIBRARIES = libabc.la
libabc_la_CFLAGS = $(ABC_INCS)
libabc_la_SOURCES = $(ABC_SRCS)
noinst_HEADERS = $(ABC_HDRS)
