#!/bin/bash

export PRIMME_VERSION=3.1.1
export PREFIX="$PWD/third_party/primme"

mkdir workdir && pushd workdir
wget "https://github.com/primme/primme/archive/v${PRIMME_VERSION}.tar.gz"
tar xf "v${PRIMME_VERSION}.tar.gz"
pushd "primme-${PRIMME_VERSION}"

export CC=gcc CFLAGS="-O3 -march=nocona -mtune=ivybridge -fPIC -DNDEBUG -DPRIMME_BLASINT_SIZE=32 -DPRIMME_INT_SIZE=64"
export FFLAGS="-fno-second-underscore -O3 -march=nocona -mtune=ivybridge"
export LDFLAGS="-Wl,-z,defs -llapack -lblas -lm"
export PRIMME_WITH_HALF=no PRIMME_WITH_FLOAT=yes
make lib solib -j8
LIBS="$LDFLAGS" make test
make install
cp "lib/libprimme.a" "${PREFIX}/lib"

mkdir -p "${PREFIX}/lib/pkgconfig"
cat >"${PREFIX}/lib/pkgconfig/primme.pc" <<-EOF
	prefix=${PREFIX}
EOF
cat >>"${PREFIX}/lib/pkgconfig/primme.pc" <<-'EOF'
	includedir=${prefix}/include
	libdir=${prefix}/lib

	Name: primme
	Description: PRIMME: PReconditioned Iterative MultiMethod Eigensolver
	URL: https://github.com/primme/primme
EOF
cat >>"${PREFIX}/lib/pkgconfig/primme.pc" <<-EOF
	Version: ${PRIMME_VERSION}
EOF
cat >>"${PREFIX}/lib/pkgconfig/primme.pc" <<-'EOF'
	Cflags: -I${includedir} -DPRIMME_INT_SIZE=64
	Libs: -L${libdir} -lprimme -Wl,-rpath=${libdir}
EOF

cd ../../
