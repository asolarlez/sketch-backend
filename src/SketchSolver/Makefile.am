# Where to find the solvers, and which one to use
SOLVERS = SolverInterfaces/
SOLVER = -DABCSAT

SOLVER_LIBS = 
SOLVER_INCS =
SOLVER_SRCS = 
include SolverInterfaces/solvers.mk

# TODO: the SBit executable is "no install" for now; it should be a library
bin_PROGRAMS = SBitII
SBitII_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -ISolverInterfaces/ -IInputParser/
SBitII_CFLAGS = $(SBitII_CXXFLAGS)
SBitII_LDADD = $(SOLVER_LIBS) -ldl
SBitII_SOURCES =				\
	$(SOLVER_SRCS)				\
	FindCheckSolver.cpp			\
	BooleanToCNF.cpp			\
	InputParser/BooleanDAG.cpp		\
	InputParser/DagCSE.cpp			\
	InputParser/DagElimUFUN.cpp		\
	InputParser/DagFunctionInliner.cpp	\
	InputParser/DagFunctionToAssertion.cpp	\
	InputParser/DagOptim.cpp		\
	InputParser/DagOptimizeCommutAssoc.cpp	\
	InputParser/driver.cpp			\
	InputParser/InputReader.cpp		\
	InputParser/Main.cpp			\
	InputParser/NodeStore.cpp		\
	InputParser/NodesToSEQ.cpp		\
	InputParser/NodesToSolver.cpp		\
	InputParser/NodesToEuclid.cpp		\
	InputParser/NodeVisitor.cpp		\
	InputParser/SolveFromInput.cpp

## Hackery to put the lex/bison generated parser into a C++ namespace

BUILT_SOURCES = InputParser.cpp InputParser.hpp InputLexer.cpp

InputParser/InputReader.cpp: InputParser.cpp InputLexer.cpp

InputLexer.cpp: InputParser/InputLexer.ll InputParser.cpp
	$(LEX) -o$@ $<

InputParser.hpp: InputParser.cpp

InputParser.cpp: InputParser/InputParser.yy
	$(YACC) -dvty -o$@ $<

clean-local:
	rm -f InputParser.* InputLexer.*
