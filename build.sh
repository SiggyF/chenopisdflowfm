#!/bin/sh

# Better install dflowfm, because otherwise it's hard to debug...
DFLOWFM_DIR="/Users/fedorbaart/Documents/checkouts/unstruc/build"
SWAN_DIR="/Users/fedorbaart/Documents/checkouts/swandeltares"
# find libswanesmf.so and libdflowfmesmf.so/dylib
export LDFLAGS="-L${DFLOWFM_DIR}/lib -L${SWAN_DIR} "
# find unstruc_esmf.mod
export FCFLAGS="-I${DFLOWFM_DIR}/include -I${SWAN_DIR} -g -O0"
# this should not be necessary
export FCFLAGS="$FCFLAGS  -ffree-line-length-none"


if [ ! -x ./configure ]
then
    ./autogen.sh
fi
./configure --with-swan --with-dflowfm
make

echo 'Remember to add the following paths to your LD_LIBRARY_PATH (linux)/DYLD_LIBRARY_PATH (osx)'
echo "SWAN: ${SWAN_DIR}"
echo "DFLOWFM: ${DFLOWFM_DIR}"
