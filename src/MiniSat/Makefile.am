noinst_LTLIBRARIES = libminisat.la
noinst_HEADERS = core/MSolver.h mtl/Alg.h mtl/BoxedVec.h mtl/Map.h mtl/Sort.h core/SolverTypes.h mtl/BasicHeap.h mtl/Heap.h mtl/Queue.h mtl/Vec.h 
 
libminisat_la_CXXFLAGS = -std=gnu++11 -Imtl -Icore -I../SketchSolver/InputParser/ -I../SketchSolver/NumericalSynthesis/ReasCore/ -I../SketchSolver/NumericalSynthesis/BoolAbstraction/ -I../SketchSolver/NumericalSynthesis/Solvers/ -I../SketchSolver/NumericalSynthesis/SymbolicEvaluators/ -I../SketchSolver/NumericalSynthesis/SymbolicAnalyzers/ -I../SketchSolver/NumericalSynthesis/DataStructures/ -I../SketchSolver/NumericalSynthesis/Optimizers/ -I../SketchSolver/NumericalSynthesis/IntervalProp/ -I../SketchSolver/NumericalSynthesis/ConflictGenerators/ -I../SketchSolver/NumericalSynthesis/SuggestionGenerators/ -I../SketchSolver/NumericalSynthesis/Utils/ -I../SketchSolver/NumericalSynthesis/ -I../SketchSolver/ -I../SketchSolver/SolverInterfaces
libminisat_la_CFLAGS = $(libminisat_a_CXXFLAGS)

libminisat_la_SOURCES = core/Solver.cpp
