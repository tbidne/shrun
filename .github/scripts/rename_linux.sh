#!/usr/bin/env bash

shrun_vers="0.9"

. /etc/lsb-release
ubuntu_vers=$(echo $DISTRIB_RELEASE)

arch=$(uname -m)

chmod a+x bin/shrun-ubuntu-latest
mv bin/shrun-ubuntu-latest "bin/shrun_$shrun_vers-$arch-linux-ubuntu_$ubuntu_vers"