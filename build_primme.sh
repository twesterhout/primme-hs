#!/bin/bash

set -e

TEST=0
VERBOSE=0
BUILD=1
while [[ $# -gt 0 ]]; do
	key="$1"
	case $key in
		--test) TEST=1; shift; ;;
		--verbose) VERBOSE=1; shift; ;;
		--clean) BUILD=0; shift; ;;
		*)
			echo "Unknown argument: $1"
			echo "Usage: ./build_primme.sh [--test] [--verbose] [--clean]"
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

mkdir -p "$WORKDIR" && cd "$WORKDIR"
if [ ! -f "v${PRIMME_VERSION}.tar.gz" ]; then
	[ $VERBOSE -eq 1 ] && echo "Downloading primme-${PRIMME_VERSION} ..."
	wget --no-verbose "https://github.com/primme/primme/archive/v${PRIMME_VERSION}.tar.gz"
fi
tar xf "v${PRIMME_VERSION}.tar.gz"
cd "primme-${PRIMME_VERSION}"

if [ $BUILD -eq 1 ]; then
	export CFLAGS="$(pkg-config --cflags openblas)"
	export CFLAGS="$CFLAGS -O3 -march=nocona -mtune=ivybridge -fPIC -DNDEBUG -DPRIMME_BLASINT_SIZE=32 -DPRIMME_INT_SIZE=64"
	export FFLAGS="-fno-second-underscore -O3 -march=nocona -mtune=ivybridge"
	export LDFLAGS="-Wl,-z,defs $(pkg-config --libs openblas) -lm"
	export LIBS="$LDFLAGS"
	export PRIMME_WITH_HALF=no PRIMME_WITH_FLOAT=yes
	run_make -j$(nproc) lib 
	[ $TEST -eq 1 ] && run_make test
	run_make install
	cp $([ $VERBOSE -eq 1 ] && echo "-v") "lib/libprimme.a" "${PREFIX}/lib"
	# We want the static lib only
	rm $([ $VERBOSE -eq 1 ] && echo "-v") "${PREFIX}"/lib/libprimme.so*
else
	run_make clean
fi
