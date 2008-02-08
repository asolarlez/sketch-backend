#!/bin/bash

SOURCES=modules.sources.mk
INCLUDES=modules.includes.mk

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

for mod in `cat modules.txt`; do
    echo -n "-I\$(ABC)/${mod} " >> $INCLUDES
    cat $mod/module.make >> $SOURCES
done
