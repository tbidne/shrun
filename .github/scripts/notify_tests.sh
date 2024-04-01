#!/usr/bin/env bash

# This script intended to be used by CI.

set -e

export LANG="C.UTF-8"

cabal update

cabal configure --enable-tests

cabal test notify
