##-----------------------------------------------------------------------------
##
## A system for declaring solvers to used in SBit.
##
## Each solver needs at least the following:
## 
##   SOLVER_LIBS += [libs]   # the object files to be linked into SBit
## 
## Optionally, one can specify:
##
##   SOLVER_SRCS += [sources]  # files compiled with SBit (e.g., interfaces)
##   SOLVER_INCS += [dirs]    # directories added to compiler's search path
##                         # (e.g., locations of header files)
##
## Files referenced from this directory should be prefixed with the 
## $(SOLVERS) variable so that this particular file can be included elsewhere.
##-----------------------------------------------------------------------------

SOLVER_LIBS = 
SOLVER_INCS =
SOLVER_SRCS = 
SOLVER_HDRS = 

## Interface
SOLVER_SRCS     += $(SOLVERS)/SATSolver.cpp
SOLVER_HDRS	+= $(SOLVERS)/SATSolver.h

## ABC
#ABC		= ../abc60513
#SOLVER_LIBS	+= $(ABC)/libabc.a
# TODO: all of ABC's source directories are included here.  ABC should
# have a narrower interface.
#include $(ABC)/modules.includes.mk
#SOLVER_INCS	+= $(ABC_INCS)
#SOLVER_SRCS	+= $(SOLVERS)/ABCSATSolver.cpp
#SOLVER_HDRS	+= $(SOLVERS)/ABCSATSolver.h

## MiniSat
SOLVER_LIBS	+= ../MiniSat/libminisat.a
SOLVER_INCS	+= -I../MiniSat/core -I../MiniSat/mtl
SOLVER_SRCS	+= $(SOLVERS)/MiniSATSolver.cpp
SOLVER_HDRS	+= $(SOLVERS)/MiniSATSolver.h

## zchaff
#SOLVER_LIBS	+= ../zchaff/libzchaff.a
#SOLVER_INCS	+= -I../zchaff
#SOLVER_SRCS	+= $(SOLVERS)/ZchaffSATSolver.cpp
#SOLVER_HDRS	+= $(SOLVERS)/ZchaffSATSolver.h
