#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="$SHRUN_VERS-$arch-linux"

docker build \
  -t shrun_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  .

cp docker_out/shrun bin/

echo "*** Testing exe ***"
./bin/shrun --version

echo "*** Computing sha256 ***"
sha256sum ./bin/shrun > ./bin/shrun.sha256
cat ./bin/shrun.sha256

# -j needed to keep structure flat
zip "shrun_$suffix.zip" -j ./bin/*
