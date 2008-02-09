#!/bin/sh

ABC_GEN=gen_makefile_includes.sh
SKETCH_GEN=gen_sketch_sources.sh

cd backend/abc60513
/bin/sh $ABC_GEN
cd ../..

cd frontend
/bin/sh $SKETCH_GEN
cd ../

autoreconf --force --install
