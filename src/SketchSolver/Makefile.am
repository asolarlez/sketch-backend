# Where to find the solvers, and which one to use
SOLVERS = SolverInterfaces/
#SOLVER = -DABCSAT

include SolverInterfaces/solvers.mk

# TODO: the SBit executable is "no install" for now; it should be a library
bin_PROGRAMS = cegis gdtest igtest snopttest snoptitest
lib_LTLIBRARIES = libcegis.la
cegis_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -std=gnu++11 -ISolverInterfaces/ -IInputParser/ -INumericalSynthesis/SymbolicEvaluators/ -INumericalSynthesis/SymbolicAnalyzers/ -INumericalSynthesis/DataStructures/ -INumericalSynthesis/Optimizers/ -INumericalSynthesis/IntervalProp/ -INumericalSynthesis/ConflictGenerators/ -INumericalSynthesis/ -I/usr/local/include -lgsl -lgslcblas -lsnopt_cpp
gdtest_CXXFLAGS = $(cegis_CXXFLAGS)
igtest_CXXFLAGS = $(cegis_CXXFLAGS)
snopttest_CXXFLAGS = $(cegis_CXXFLAGS)
snoptitest_CXXFLAGS = $(cegis_CXXFLAGS)
#cegis_CFLAGS = $(cegis_CXXFLAGS)
libcegis_la_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -std=gnu++11 -ISolverInterfaces/ -IInputParser/ -INumericalSynthesis/SymbolicEvaluators/ -INumericalSynthesis/SymbolicAnalyzers/ -INumericalSynthesis/DataStructures/ -INumericalSynthesis/Optimizers/ -INumericalSynthesis/IntervalProp/ -INumericalSynthesis/ConflictGenerators/ -INumericalSynthesis/ -I/usr/local/include -lgsl -lgslcblas
libcegis_la_CFLAGS = $(cegis_CXXFLAGS)
cegis_LDADD = libcegis.la
gdtest_LDADD = libcegis.la
igtest_LDADD = libcegis.la
snopttest_LDADD = libcegis.la
snoptitest_LDADD = libcegis.la
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
  NumericalSynthesis/NumericalSolver.h     \
	NumericalSynthesis/NumericalSolverHelper.h	\
	NumericalSynthesis/BasicNumericalHelper.h		\
    NumericalSynthesis/IteApproxNumericalHelper.h   \
	NumericalSynthesis/InequalityHelper.h	\
	NumericalSynthesis/BoolApproxHelper.h	\
    NumericalSynthesis/SmoothSatHelper.h    \
	NumericalSynthesis/GradUtil.h		\
	NumericalSynthesis/Util.h	\
	NumericalSynthesis/Optimizers/GradientDescent.h			\
	NumericalSynthesis/Optimizers/Snopt.h			\
	NumericalSynthesis/Optimizers/OptimizationWrapper.h	\
	NumericalSynthesis/Optimizers/GradientDescentWrapper.h	\
	NumericalSynthesis/Optimizers/SnoptWrapper.h	\
	NumericalSynthesis/Optimizers/SimulatedAnnealing.h		\
	NumericalSynthesis/SymbolicEvaluators/SymbolicEvaluator.h		\
    NumericalSynthesis/SymbolicEvaluators/IteAutoDiff.h     \
	NumericalSynthesis/SymbolicEvaluators/AutoDiff.h \
	NumericalSynthesis/SymbolicEvaluators/BoolAutoDiff.h	\
	NumericalSynthesis/SymbolicEvaluators/RangeDiff.h   \
	NumericalSynthesis/SymbolicEvaluators/SimpleEvaluator.h	\
	NumericalSynthesis/DataStructures/DistanceGrad.h		\
	NumericalSynthesis/DataStructures/ValueGrad.h \
	NumericalSynthesis/DataStructures/Interval.h		\
	NumericalSynthesis/DataStructures/IntervalGrad.h		\
	NumericalSynthesis/IntervalProp/IntervalPropagator.h	\
	NumericalSynthesis/ConflictGenerators/ConflictGenerator.h	\
    NumericalSynthesis/SymbolicAnalyzers/GradientAnalyzer.h  \
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
	InputParser/guardedVal.h			\
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
	InputParser/NodeHardcoder.h			\
	InputParser/HoleHardcoder.h			\
	InputParser/guardedVal.h			\
    InputParser/FloatSupport.h

libcegis_la_SOURCES =				\
	$(SOLVER_SRCS)				\
	BooleanToCNF.cpp			\
	FindCheckSolver.cpp			\
	memory_sampler.cc			\
  NumericalSynthesis/NumericalSolver.cpp \
	NumericalSynthesis/BasicNumericalHelper.cpp		\
    NumericalSynthesis/IteApproxNumericalHelper.cpp \
	NumericalSynthesis/InequalityHelper.cpp		\
	NumericalSynthesis/BoolApproxHelper.cpp		\
    NumericalSynthesis/SmoothSatHelper.cpp      \
	NumericalSynthesis/GradUtil.cpp	\
	NumericalSynthesis/Optimizers/GradientDescent.cpp	\
	NumericalSynthesis/Optimizers/Snopt.cpp	\
	NumericalSynthesis/DataStructures/DistanceGrad.cpp		\
	NumericalSynthesis/DataStructures/ValueGrad.cpp	\
	NumericalSynthesis/DataStructures/Interval.cpp \
	NumericalSynthesis/DataStructures/IntervalGrad.cpp \
    NumericalSynthesis/SymbolicEvaluators/IteAutoDiff.cpp   \
	NumericalSynthesis/SymbolicEvaluators/BoolAutoDiff.cpp	\
	NumericalSynthesis/SymbolicEvaluators/AutoDiff.cpp \
	NumericalSynthesis/SymbolicEvaluators/RangeDiff.cpp \
	NumericalSynthesis/SymbolicEvaluators/SimpleEvaluator.cpp	\
    NumericalSynthesis/SymbolicAnalyzers/GradientAnalyzer.cpp    \
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
	InputParser/NodeHardcoder.cpp  \
	InputParser/HoleHardcoder.cpp \
	InputParser/Tvalue.cpp


cegis_SOURCES =	InputParser/CegisMain.cpp
gdtest_SOURCES = NumericalSynthesis/Test/TestGradientDescent.cpp
igtest_SOURCES = NumericalSynthesis/Test/TestIntervalArith.cpp
snopttest_SOURCES = NumericalSynthesis/Test/TestSnopt.cpp
snoptitest_SOURCES = NumericalSynthesis/Test/TestSnoptInterface.cpp

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
