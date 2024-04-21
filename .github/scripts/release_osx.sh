#!/usr/bin/env bash

set -e

shrun_vers="0.9"

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  cabal_build_dir="aarch64-osx"
else
  cabal_build_dir="$arch-osx"
fi

mkdir -p bin

# see NOTE: [Cabal Build vs. Install]
#
# Use cabal build for now for symmetry with linux static release.
cabal update
cabal build exe:shrun --ghc-options -Werror

cp ./dist-newstyle/build/$cabal_build_dir/ghc-*/shrun-*/x/shrun/opt/build/shrun/shrun "bin/shrun_$shrun_vers-$arch-macos_$apple_vers-darwin"
