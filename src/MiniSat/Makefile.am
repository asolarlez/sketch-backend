noinst_LIBRARIES = libminisat.a
noinst_HEADERS = core/MSolver.h mtl/Alg.h mtl/BoxedVec.h mtl/Map.h mtl/Sort.h core/SolverTypes.h mtl/BasicHeap.h mtl/Heap.h mtl/Queue.h mtl/Vec.h 
 
libminisat_a_CXXFLAGS = -std=gnu++11 -Imtl -Icore -I../SketchSolver/InputParser/ -I../SketchSolver/ -I../SketchSolver/SolverInterfaces
libminisat_a_CFLAGS = $(libminisat_a_CXXFLAGS)

libminisat_a_SOURCES = core/Solver.cpp core/IntPropagator.cpp
