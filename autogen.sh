#!/bin/sh

ABC_GEN=gen_makefile_includes.sh


cd backend/abc60513
/bin/sh $ABC_GEN
cd ../..

autoreconf --force --install
