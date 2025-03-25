#!/usr/bin/env bash

set -e

shrun_vers="0.9.2"

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="_$shrun_vers-$arch-linux-static"

docker build \
  -t shrun_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg suffix=$suffix \
  .

cp docker_out/shrun_* bin/

echo "*** Testing exe ***"
./bin/shrun$suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/shrun$suffix > ./bin/shrun$suffix.sha256
cat ./bin/shrun$suffix.sha256
