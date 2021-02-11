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
USE_OPENBLAS=0
USE_ACCELERATE=0
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
  --use-accelerate)
    [[ "$OSTYPE" == "darwin"* ]] && USE_ACCELERATE=1
    shift
    ;;
  --use-openblas)
    [[ "$OSTYPE" != "darwin"* ]] && USE_OPENBLAS=1
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

run_make() { make $([[ $VERBOSE -ne 1 ]] && echo "-s") "$@"; }
run_cp() { cp $([[ $VERBOSE -eq 1 ]] && echo "-v") "$@"; }
run_rm() { rm $([[ $VERBOSE -eq 1 ]] && echo "-v") "$@"; }
run_echo() { [[ $VERBOSE -eq 1 ]] && echo "$@"; }

PRIMME_VERSION=3.1.1
export PREFIX="$PWD/third_party/primme"
WORKDIR="$PWD/third_party/build"

run_echo "Running on $OSTYPE..."
if [[ "$OSTYPE" == "darwin"* ]]; then
  NPROC=$(sysctl -n hw.logicalcpu)
else
  NPROC=$(nproc)
fi

mkdir -p "$WORKDIR" && cd "$WORKDIR"
if [ ! -f "v${PRIMME_VERSION}.tar.gz" ]; then
  run_echo "Downloading primme-${PRIMME_VERSION} ..."
  wget --no-verbose "https://github.com/primme/primme/archive/v${PRIMME_VERSION}.tar.gz"
fi
tar xf "v${PRIMME_VERSION}.tar.gz"
cd "primme-${PRIMME_VERSION}"
if [[ "$OSTYPE" == "darwin"* ]]; then
  patch -u include/primme.h "${PROJECT_DIR}/cbits/primme.h.patch"
elif [[ "$OSTYPE" == "linux-musl"* ]]; then
  patch -u src/include/common.h "${PROJECT_DIR}/cbits/common.h.patch"
fi

find_blas() {
  if [[ $USE_ACCELERATE -eq 1 ]]; then
    export LDFLAGS="$LDFLAGS -framework Accelerate"
  elif [[ $USE_OPENBLAS -eq 1 ]]; then
    export CFLAGS="$CFLAGS $(pkg-config --cflags openblas)"
    export LDFLAGS="$LDFLAGS $(pkg-config --libs openblas)"
  else
    export CFLAGS="$CFLAGS $(pkg-config --cflags lapack --cflags blas)"
    export LDFLAGS="$LDFLAGS $(pkg-config --libs lapack --libs blas)"
  fi
}

if [[ $BUILD -eq 1 ]]; then
  export CFLAGS="-O3 -march=nocona -mtune=haswell -fPIC -DNDEBUG -DPRIMME_BLASINT_SIZE=32 -DPRIMME_INT_SIZE=64"
  export FFLAGS="-fno-second-underscore -O3 -march=nocona -mtune=haswell"
  export PRIMME_WITH_HALF=no PRIMME_WITH_FLOAT=yes
  find_blas
  export LIBS="$LDFLAGS"
  run_echo "Using CFLAGS: $CFLAGS"
  run_echo "Using FFLAGS: $FFLAGS"
  run_echo "Using LDFLAGS: $LDFLAGS"
  run_make -j$NPROC lib
  [[ $TEST -eq 1 ]] && run_make test
  run_make install
  run_cp "lib/libprimme.a" "${PREFIX}/lib"
  # if [[ $SHARED -eq 0 ]]; then # We want the static lib only
  #   run_cp "lib/libprimme.a" "${PREFIX}/lib"
  #   if [[ "$OSTYPE" == "darwin"* ]]; then
  #     run_rm "${PREFIX}"/lib/libprimme*.dylib
  #   else
  #     run_rm "${PREFIX}"/lib/libprimme.so*
  #   fi
  # fi
else
  run_make clean
fi
