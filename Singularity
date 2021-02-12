Bootstrap: library
From: twesterhout/default/alpine-openblas:latest
Stage: build_openblas

Bootstrap: docker
From: utdemir/ghc-musl:v17-ghc884
Stage: base

%files from build_openblas
    /usr/lib/libopenblas*          /usr/lib/
    /usr/include/cblas.h           /usr/include/
    /usr/include/f77blas.h         /usr/include/
    /usr/include/lapack.h          /usr/include/
    /usr/include/openblas_config.h /usr/include/
    /usr/lib/pkgconfig/blas.pc     /usr/lib/pkgconfig/blas.pc
    /usr/lib/pkgconfig/lapack.pc   /usr/lib/pkgconfig/lapack.pc

%post
    set -eu
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal

    apk update
    # Stuff we need to build PRIMME
    apk add --no-cache build-base gfortran linux-headers python3
    # Stuff we need to build primme-hs
    apk add --no-cache git pkgconfig
    # Update Cabal stuff
    cabal v2-update

    mkdir -p /project/primme-hs.local

    # cd /project
    # git clone --depth=1 https://github.com/twesterhout/primme-hs.git
    # cd primme-hs/
    # cabal v2-build --enable-executable-static

%environment
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal
    export TERM=xterm-256color
