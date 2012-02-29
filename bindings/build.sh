#!/usr/bin/zsh
cd $(dirname $(readlink -f $0))
cd ..
make -j8 &&
gcc src/SketchSolver/InputParser/CegisCApi.h && (
    cd bindings;
    (cd CegisCApi && c2hs ../../src/SketchSolver/InputParser/CegisCApi.h API.chs) &&
    # cat CegisCApi.hs &&
    mkdir -p doc &&
    haddock --html -o doc CegisCApi/*.hs &&
    echo -e "\n\n\n\nBindings compiled successfully! Type ':browse' to see exposed functions, or open up doc/index.html\n" &&
    ghci -L../src/SketchSolver/.libs -lcegis CegisCApi.Test)
