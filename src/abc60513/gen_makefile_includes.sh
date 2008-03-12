#!/bin/bash

SOURCES=modules.sources.mk
INCLUDES=modules.includes.mk
HEADERS=modules.headers.mk

## Generate the files:
##
##   modules.includes.mk: a list of all ABC include directories
##
##   modules.sources.mk: a list of all ABC source files
##
## Both of these files are parameterized by the variable $(ABC), the location
## of the ABC source directory.
##

echo -n 'ABC_INCS = ' > $INCLUDES
echo 'ABC_SRCS = ' > $SOURCES
echo -n 'ABC_HDRS = ' > $HEADERS

## For POSIX compatibility, we have to replace this nice line:
##
##   find src/ -name '*.h' -printf '$(ABC)/src/%P ' >> $HEADERS
##
## with what follows.
for header in `find src/ -name '*.h'`; do
    echo -n "\$(ABC)/${header} " >> $HEADERS
done
echo >> $HEADERS

for mod in `cat modules.txt`; do
    echo -n "-I\$(ABC)/${mod} " >> $INCLUDES
    cat $mod/module.make >> $SOURCES
done
