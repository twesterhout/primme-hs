#!/bin/bash

set -e
set -o pipefail

SCRIPT_DIR="$(
  cd "$(dirname "$0")"
  pwd -P
)"
PROJECT_DIR="$(
  cd "${SCRIPT_DIR}"
  pwd -P
)"
BLAS=blas
SHARED=0
TEST=0
VERBOSE=0
BUILD=1
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
  --shared)
    SHARED=1
    shift
    ;;
  --blas=*)
    BLAS=${key#*=}
    shift
    ;;
  --test)
    TEST=1
    shift
    ;;
  --verbose)
    VERBOSE=1
    shift
    ;;
  --clean)
    BUILD=0
    shift
    ;;
  *)
    echo "Unknown argument: $1"
    echo "Usage: ./build_primme.sh [--shared] [--test] [--verbose] [--clean]"
    exit 1
    ;;
  esac
done

run_make() {
  make $([ $VERBOSE -ne 1 ] && echo "-s") "$@"
}

PRIMME_VERSION=3.1.1
export PREFIX="$PWD/third_party/primme"
WORKDIR="$PWD/third_party/build"

[ $VERBOSE -eq 1 ] && echo "Running on $OSTYPE..."
if [[ "$OSTYPE" == "darwin"* ]]; then
  NPROC=$(sysctl -n hw.logicalcpu)
else
  NPROC=$(nproc)
fi

mkdir -p "$WORKDIR" && cd "$WORKDIR"
if [ ! -f "v${PRIMME_VERSION}.tar.gz" ]; then
  [ $VERBOSE -eq 1 ] && echo "Downloading primme-${PRIMME_VERSION} ..."
  wget --no-verbose "https://github.com/primme/primme/archive/v${PRIMME_VERSION}.tar.gz"
fi
tar xf "v${PRIMME_VERSION}.tar.gz"
cd "primme-${PRIMME_VERSION}"
if [[ "$OSTYPE" == "darwin"* ]]; then
  patch -u include/primme.h "${PROJECT_DIR}/cbits/primme.h.patch"
fi

find_blas() {
  if [ $SHARED -eq 0 ]; then
    args="--static"
  else
    args=""
  fi
  [[ $BLAS == "accelerate" && "$OSTYPE" != "darwin"* ]] && BLAS=blas
  case "$BLAS" in
  accelerate)
    export LDFLAGS="$LDFLAGS -framework Accelerate"
    ;;
  blas)
    export CFLAGS="$CFLAGS $(pkg-config --cflags lapack --cflags blas)"
    export LDFLAGS="$LDFLAGS $(pkg-config $args --libs lapack --libs blas)"
    ;;
  openblas)
    export CFLAGS="$CFLAGS $(pkg-config --cflags openblas)"
    export LDFLAGS="$LDFLAGS $(pkg-config $args --libs openblas)"
    ;;
  *)
    echo "Invalid blas: $BLAS"
    echo "Supported values are 'accelerate', 'openblas', 'blas'"
    exit 1
    ;;
  esac
}

if [ $BUILD -eq 1 ]; then
  export CFLAGS="-O3 -march=nocona -mtune=haswell -fPIC -DNDEBUG -DPRIMME_BLASINT_SIZE=32 -DPRIMME_INT_SIZE=64"
  export FFLAGS="-fno-second-underscore -O3 -march=nocona -mtune=haswell"
  export PRIMME_WITH_HALF=no PRIMME_WITH_FLOAT=yes
  find_blas
  run_make -j$NPROC lib
  [ $TEST -eq 1 ] && run_make test
  run_make install
  if [[ $SHARED -eq 0 ]]; then # We want the static lib only
    cp $([ $VERBOSE -eq 1 ] && echo "-v") "lib/libprimme.a" "${PREFIX}/lib"
    if [[ "$OSTYPE" == "darwin"* ]]; then
      rm $([ $VERBOSE -eq 1 ] && echo "-v") "${PREFIX}"/lib/libprimme*.dylib
    else
      rm $([ $VERBOSE -eq 1 ] && echo "-v") "${PREFIX}"/lib/libprimme.so*
    fi
  fi
  [ $VERBOSE -eq 1 ] && ls -l "${PREFIX}/lib"
else
  run_make clean
fi
