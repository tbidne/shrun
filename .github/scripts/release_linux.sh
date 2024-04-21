#!/usr/bin/env bash

set -e

shrun_vers="0.9"

. /etc/lsb-release
ubuntu_vers=$(echo $DISTRIB_RELEASE)

arch=$(uname -m)

mkdir -p bin

# see NOTE: [Cabal Build vs. Install]
#
# Use cabal build for now for symmetry with linux static release.
cabal update
cabal build exe:shrun --ghc-options -Werror

cp ./dist-newstyle/build/x86_64-linux/ghc-*/shrun-*/x/shrun/opt/build/shrun/shrun "bin/shrun_$shrun_vers-$arch-linux-ubuntu_$ubuntu_vers"
