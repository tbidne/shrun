#!/usr/bin/env bash

# This script intended to be used by CI.

set -e

verify_var() {
  var_name=$1
  # We want to take in a string like 'GHC_VERS' then eval it like it's a
  # variable. We do this so we can report the actual name in the error message.
  # Probably there is a better way to do this.
  var_result=$(eval "echo \$$var_name")
  if [[ -z $var_result ]]; then
    echo "$var_name env var not set"
    return 1
  else
    echo "$var_name is set: $var_result"
  fi
}

verify_var "SHRUN_VERS"
verify_var "GHC_VERS"
verify_var "CABAL_VERS"
verify_var "CABAL_PROJ"
