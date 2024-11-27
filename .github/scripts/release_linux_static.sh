#!/usr/bin/env bash

set -e

shrun_vers="0.9.2"

dir=$1

mkdir -p bin

docker build \
  -t shrun_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg shrun_vers=$shrun_vers \
  .

cp docker_out/shrun_* bin/
