# Where to find the solvers, and which one to use
SOLVERS = SolverInterfaces/
#SOLVER = -DABCSAT

include SolverInterfaces/solvers.mk

# TODO: the SBit executable is "no install" for now; it should be a library
bin_PROGRAMS = cegis
lib_LTLIBRARIES = libcegis.la
cegis_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -ISolverInterfaces/ -IInputParser/
#cegis_CFLAGS = $(cegis_CXXFLAGS)
libcegis_la_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -ISolverInterfaces/ -IInputParser/
libcegis_la_CFLAGS = $(cegis_CXXFLAGS)
cegis_LDADD = libcegis.la
#libcegis_la_LIBADD = ../abc60513/libabc.la ../MiniSat/libminisat.la ../zchaff/libzchaff.la -ldl
libcegis_la_LIBADD = ../MiniSat/libminisat.la -ldl
libcegis_la_LDFLAGS = -version-infp 1:0:0
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
	InputParser/CegisCApi.h					\
	InputParser/CEGISSolver.h \
	InputParser/CallGraphAnalysis.h \
	InputParser/Checkpointer.h \
	InputParser/CommandLineArgs.h			\
	InputParser/ComplexInliner.h \
	InputParser/DagCSE.h				\
	InputParser/DagElimUFUN.h			\
	InputParser/DagFunctionInliner.h		\
	InputParser/DagFunctionToAssertion.h		\
	InputParser/DagOptim.h				\
	InputParser/DagOptimizeCommutAssoc.h		\
	InputParser/ExtractEvaluationCondition.h	\
	InputParser/FastSet.h				\
	InputParser/InputReader.h			\
	InputParser/InterpreterEnvironment.h \
	InputParser/CegisMainHelpers.h \
	InputParser/NodeEvaluator.h \
	InputParser/NodeSlicer.h \
	InputParser/NodeStore.h				\
	InputParser/NodeVisitor.h			\
	InputParser/NodesToEuclid.h			\
	InputParser/NodesToSolver.h			\
	InputParser/SolveFromInput.h			\
	InputParser/Tvalue.h				\
	InputParser/VarStore.h	\
	InputParser/driver.h				\
	InputParser/guardedVal.h			

libcegis_la_SOURCES =				\
	$(SOLVER_SRCS)				\
	BooleanToCNF.cpp			\
	FindCheckSolver.cpp			\
	memory_sampler.cc			\
	InputParser/BackwardsAnalysis.cpp	\
	InputParser/BooleanDAG.cpp		\
	InputParser/BooleanDAGCreator.cpp	\
	InputParser/BooleanNodes.cpp		\
	InputParser/CegisCApi.cpp			\
	InputParser/CegisMainHelpers.cpp			\
	InputParser/CEGISSolver.cpp	\
	InputParser/CallGraphAnalysis.cpp \
	InputParser/Checkpointer.cpp \
	InputParser/ComplexInliner.cpp \
	InputParser/CommandLineArgs.cpp \
	InputParser/DagCSE.cpp			\
	InputParser/DagElimUFUN.cpp		\
	InputParser/DagFunctionInliner.cpp	\
	InputParser/DagFunctionToAssertion.cpp	\
	InputParser/DagOptim.cpp		\
	InputParser/DagOptimizeCommutAssoc.cpp	\
	InputParser/driver.cpp			\
	InputParser/InputReader.cpp		\
	InputParser/InterpreterEnvironment.cpp \
	InputParser/NodeEvaluator.cpp \
	InputParser/NodeStore.cpp		\
	InputParser/NodeSlicer.cpp \
	InputParser/NodesToSolver.cpp		\
	InputParser/NodesToEuclid.cpp		\
	InputParser/NodeVisitor.cpp		\
	InputParser/SolveFromInput.cpp \
    InputParser/Tvalue.cpp


cegis_SOURCES =	InputParser/CegisMain.cpp			

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
