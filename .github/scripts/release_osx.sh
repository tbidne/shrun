#!/usr/bin/env bash

set -e

shrun_vers="0.9.2"

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  # standardize name
  arch="aarch64"
fi

# x86_64-osx on macos-12/13, aarch64-osx on macos-14/15
cabal_build_dir="$arch-osx"

mkdir -p bin

suffix="_$shrun_vers-$arch-macos_$apple_vers-darwin"

cabal update
cabal install exe:shrun --installdir bin/ --program-suffix $suffix --project-file cabal.ghc982.project --ghc-options -Werror

echo "*** Testing exe ***"
./bin/shrun$suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/shrun$suffix > ./bin/shrun$suffix.sha256
cat ./bin/shrun$suffix.sha256
