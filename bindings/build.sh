#!/usr/bin/zsh
cd $(dirname $(readlink -f $0))
cd ..
make -j8 &&
gcc src/SketchSolver/InputParser/CegisCApi.h && (
    cd bindings;
    (cd CegisCApi && c2hs ../../src/SketchSolver/InputParser/CegisCApi.h API.chs) &&
    mkdir -p doc &&
    haddock --html -o doc CegisCApi/*.hs &&
    mkdir -p .libs &&
    cp ../src/SketchSolver/.libs/libcegis.a .libs &&
    mkdir -p dist &&
    ghc -outputdir dist -o hscegis -O2 -optc-O3 -fforce-recomp -main-is HsCegis HsCegis -L.libs -lcegis -lstdc++
    )
