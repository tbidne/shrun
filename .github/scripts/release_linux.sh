#!/usr/bin/env bash

set -e

shrun_vers="0.9.1"

. /etc/lsb-release
ubuntu_vers=$(echo $DISTRIB_RELEASE)

arch=$(uname -m)

mkdir -p bin

cabal update
cabal install exe:shrun --installdir bin/ --program-suffix "_$shrun_vers-$arch-linux-ubuntu_$ubuntu_vers" --project-file cabal.ghc982.project --ghc-options -Werror

