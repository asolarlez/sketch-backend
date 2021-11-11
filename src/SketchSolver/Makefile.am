# Where to find the solvers, and which one to use
SOLVERS = SolverInterfaces/
#SOLVER = -DABCSAT

include SolverInterfaces/solvers.mk

# TODO: the SBit executable is "no install" for now; it should be a library
bin_PROGRAMS = cegis
noinst_LIBRARIES = libcegis.a
cegis_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -std=gnu++17 -D_NOGSL -D_NOSNOPT -ISolverInterfaces/ -IInputParser/ -INumericalSynthesis/ReasCore/ -INumericalSynthesis/BoolAbstraction/ -INumericalSynthesis/Solvers/ -INumericalSynthesis/Samplers/ -INumericalSynthesis/SymbolicEvaluators/ -INumericalSynthesis/SymbolicAnalyzers/ -INumericalSynthesis/DataStructures/ -INumericalSynthesis/Optimizers/ -INumericalSynthesis/IntervalProp/ -INumericalSynthesis/ConflictGenerators/ -INumericalSynthesis/SuggestionGenerators/ -INumericalSynthesis/Utils/ -INumericalSynthesis/ -I/usr/local/include
#gdtest_CXXFLAGS = $(cegis_CXXFLAGS)
#igtest_CXXFLAGS = $(cegis_CXXFLAGS)
#snopttest_CXXFLAGS = $(cegis_CXXFLAGS)
#snoptitest_CXXFLAGS = $(cegis_CXXFLAGS)
#cegis_CFLAGS = $(cegis_CXXFLAGS)

libcegis_a_CXXFLAGS = $(SOLVER) $(SOLVER_INCS) -std=gnu++17 -Wno-narrowing  -D_NOGSL -D_NOSNOPT  -ISolverInterfaces/ -IInputParser/ -INumericalSynthesis/ReasCore/ -INumericalSynthesis/BoolAbstraction/ -INumericalSynthesis/Solvers/ -INumericalSynthesis/Samplers/ -INumericalSynthesis/SymbolicEvaluators/ -INumericalSynthesis/SymbolicAnalyzers/ -INumericalSynthesis/DataStructures/ -INumericalSynthesis/Optimizers/ -INumericalSynthesis/IntervalProp/ -INumericalSynthesis/ConflictGenerators/ -INumericalSynthesis/SuggestionGenerators/ -INumericalSynthesis/Utils/ -INumericalSynthesis/ -I/usr/local/include
# -lgsl -lgslcblas -lsnopt_cpp
libcegis_a_CFLAGS = $(cegis_CXXFLAGS)
cegis_LDADD = libcegis.a ../MiniSat/libminisat.a
#libcegis_a_LDFLAGS = -version-infp 1:0:0
cegis_LDFLAGS = $(AM_LDFLAGS) -static
#snopttest_LDADD = libcegis.a
#snoptitest_LDADD = libcegis.a
#libcegis_la_LDFLAGS = -version-infp 1:0:0
cegis_LDFLAGS = $(AM_LDFLAGS) -static
EXTRA_DIST = InputParser/InputLexer.ll InputParser/InputParser.yy
noinst_HEADERS = $(SOLVER_HDRS)				\
	BasicError.h					\
	BooleanToCNF.h					\
	FindCheckSolver.h				\
	memory_statistics.h				\
	memory_sampler.h				\
	timerclass.h					\
	EntityResolutionSolver.h  			\
	GTPredicateSolver.h				\
    NumericalSynthesis/ReasCore/REASSolver.h       \
    NumericalSynthesis/ReasCore/NumDebugger.h 		\
    NumericalSynthesis/BoolAbstraction/BoolAbstractor.h \
	ArithmeticExpressionBuilder.h		\
	ArithmeticExpressionSolver.h     				\
    NumericalSynthesis/BoolAbstraction/BoolNodeSimplifier.h \
    NumericalSynthesis/BoolAbstraction/Predicate.h 	\
    NumericalSynthesis/BoolAbstraction/Interface.h  \
    NumericalSynthesis/ReasCore/NumericalSynthesizer.h     \
	NumericalSynthesis/Solvers/NumericalSolver.h	\
 	NumericalSynthesis/Utils/GradUtil.h		\
	NumericalSynthesis/Utils/Util.h	\
	NumericalSynthesis/Optimizers/OptSolver.h 		\
	NumericalSynthesis/Optimizers/Snopt.h			\
    NumericalSynthesis/Optimizers/GradientDescent.h    \
	NumericalSynthesis/Optimizers/OptimizationWrapper.h	\
	NumericalSynthesis/Optimizers/MaxOptimizationWrapper.h \
	NumericalSynthesis/Optimizers/SnoptWrapper.h	\
	NumericalSynthesis/Optimizers/GradientDescentWrapper.h \
	NumericalSynthesis/Optimizers/MaxSolver.h		\
	NumericalSynthesis/Optimizers/MaxSolverWrapper.h \
	NumericalSynthesis/SymbolicEvaluators/SymbolicEvaluator.h		\
	NumericalSynthesis/SymbolicEvaluators/BoolAutoDiff.h	\
	NumericalSynthesis/SymbolicEvaluators/KLocalityAutoDiff.h \
	NumericalSynthesis/SymbolicEvaluators/SimpleEvaluator.h	\
	NumericalSynthesis/SymbolicEvaluators/SimpleGradEvaluator.h \
	NumericalSynthesis/SymbolicEvaluators/ActualEvaluators.h \
	NumericalSynthesis/SymbolicEvaluators/SmoothEvaluators.h \
	NumericalSynthesis/SymbolicEvaluators/SmoothAutoDiff.h \
	NumericalSynthesis/DataStructures/DistanceGrad.h		\
	NumericalSynthesis/DataStructures/ValueGrad.h \
	NumericalSynthesis/DataStructures/Interval.h		\
	NumericalSynthesis/DataStructures/IntervalGrad.h		\
	NumericalSynthesis/IntervalProp/IntervalPropagator.h	\
	NumericalSynthesis/ConflictGenerators/ConflictGenerator.h	\
    NumericalSynthesis/ConflictGenerators/SimpleConflictGenerator.h	\
    NumericalSynthesis/SuggestionGenerators/SuggestionGenerator.h	\
    NumericalSynthesis/SuggestionGenerators/SimpleSuggestionGenerator.h	\
    NumericalSynthesis/SuggestionGenerators/SmartSuggestionGenerator1.h \
    NumericalSynthesis/SuggestionGenerators/SuggestionGeneratorUsingMax.h \
    NumericalSynthesis/Samplers/Sampler.h 	\
    NumericalSynthesis/Samplers/BasicSampler.h \
    NumericalSynthesis/Samplers/BoolBasedSampler.h \
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
	InputParser/NodeHardcoder.h			\
	InputParser/HoleHardcoder.h			\
	InputParser/guardedVal.h			\
    InputParser/FloatSupport.h 			\
    InputParser/IntToFloatRewriteDag.h \
    InputParser/CEGISChecker.h \
    InputParser/CEGISFinder.h \
    InputParser/CounterexampleFinder.h

libcegis_a_SOURCES =				\
	$(SOLVER_SRCS)				\
	BooleanToCNF.cpp			\
	FindCheckSolver.cpp			\
	memory_sampler.cc			\
	ArithmeticExpressionBuilder.cpp		\
    NumericalSynthesis/ReasCore/REASSolver.cpp  \
    NumericalSynthesis/BoolAbstraction/BoolAbstractor.cpp \
    NumericalSynthesis/BoolAbstraction/BoolNodeSimplifier.cpp \
    NumericalSynthesis/BoolAbstraction/Predicate.cpp 	\
    NumericalSynthesis/ReasCore/NumericalSynthesizer.cpp \
    NumericalSynthesis/Solvers/NumericalSolver.cpp  \
	NumericalSynthesis/Utils/GradUtil.cpp	\
	NumericalSynthesis/Optimizers/Snopt.cpp	\
    NumericalSynthesis/Optimizers/GradientDescent.cpp       \
    NumericalSynthesis/Optimizers/MaxSolver.cpp 			\
	NumericalSynthesis/DataStructures/DistanceGrad.cpp		\
	NumericalSynthesis/DataStructures/ValueGrad.cpp	\
	NumericalSynthesis/DataStructures/Interval.cpp \
	NumericalSynthesis/DataStructures/IntervalGrad.cpp \
	NumericalSynthesis/SymbolicEvaluators/BoolAutoDiff.cpp	\
	NumericalSynthesis/SymbolicEvaluators/SmoothAutoDiff.cpp \
	NumericalSynthesis/SymbolicEvaluators/KLocalityAutoDiff.cpp \
   	NumericalSynthesis/SymbolicEvaluators/SimpleEvaluator.cpp	\
   	NumericalSynthesis/SymbolicEvaluators/SimpleGradEvaluator.cpp \
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
	InputParser/HoleHardcoder.cpp             \
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
	InputParser/Tvalue.cpp \
    InputParser/CEGISChecker.cpp \
    InputParser/CEGISFinder.cpp \
    InputParser/CounterexampleFinder.cpp \
    InputParser/SkVal.cpp



cegis_SOURCES =	InputParser/CegisMain.cpp
#gdtest_SOURCES = NumericalSynthesis/Test/TestGradientDescent.cpp
#igtest_SOURCES = NumericalSynthesis/Test/TestIntervalArith.cpp
#snopttest_SOURCES = NumericalSynthesis/Test/TestSnopt.cpp
#snoptitest_SOURCES = NumericalSynthesis/Test/TestSnoptInterface.cpp

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
