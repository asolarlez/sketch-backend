# Where to find the solvers, and which one to use
SOLVERS = SolverInterfaces/
SOLVER = -DABCSAT

include SolverInterfaces/solvers.mk

# TODO: the SBit executable is "no install" for now; it should be a library
bin_PROGRAMS = SBitII
SBitII_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -ISolverInterfaces/ -IInputParser/
SBitII_CFLAGS = $(SBitII_CXXFLAGS)
SBitII_LDADD = $(SOLVER_LIBS) -ldl
EXTRA_DIST = InputParser/InputLexer.ll InputParser/InputParser.yy
noinst_HEADERS = $(SOLVER_HDRS)				\
	BasicError.h					\
	BooleanToCNF.h					\
	FindCheckSolver.h				\
	memory_statistics.h				\
	memory_sampler.h				\
	timerclass.h					\
	InputParser/BackwardsAnalysis.h			\
	InputParser/BooleanDAG.h			\
	InputParser/BooleanDAGCreator.h			\
	InputParser/BooleanNodes.h			\
	InputParser/CommandLineArgs.h			\
	InputParser/DagCSE.h				\
	InputParser/DagElimUFUN.h			\
	InputParser/DagFunctionInliner.h		\
	InputParser/DagFunctionToAssertion.h		\
	InputParser/DagOptim.h				\
	InputParser/DagOptimizeCommutAssoc.h		\
	InputParser/ExtractEvaluationCondition.h	\
	InputParser/FastSet.h				\
	InputParser/InputReader.h			\
	InputParser/NodeStore.h				\
	InputParser/NodeVisitor.h			\
	InputParser/NodesToEuclid.h			\
	InputParser/NodesToSEQ.h			\
	InputParser/NodesToSolver.h			\
	InputParser/SFIOutputSeq.h			\
	InputParser/SolveFromInput.h			\
	InputParser/Tvalue.h				\
	InputParser/driver.h

SBitII_SOURCES =				\
	$(SOLVER_SRCS)				\
	BooleanToCNF.cpp			\
	FindCheckSolver.cpp			\
	memory_sampler.cc			\
	InputParser/BackwardsAnalysis.cpp	\
	InputParser/BooleanDAG.cpp		\
	InputParser/BooleanDAGCreator.cpp	\
	InputParser/BooleanNodes.cpp		\
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
