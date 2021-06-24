#!/bin/sh

any_failed=0

echo "*** CHECKING CABAL FORMAT ***"
cabal_failed=0
./ci_scripts/cabal-fmt.sh
if [[ $? -ne 0 ]]; then
  cabal_failed=1
  any_failed=1
fi

echo "*** CHECKING NIX FORMAT ***"
nix_failed=0
./ci_scripts/nixpkgs-fmt.sh
if [[ $? -ne 0 ]]; then
  nix_failed=1
  any_failed=1
fi

echo "*** CHECKING HASKELL FORMAT ***"
ormolu_failed=0
./ci_scripts/ormolu.sh
if [[ $? -ne 0 ]]; then
  ormolu_failed=1
  any_failed=1
fi

echo "*** CHECKING HLINT HINTS ***"
hlint_failed=0
./ci_scripts/hlint.sh
if [[ $? -ne 0 ]]; then
  hlint_failed=1
  any_failed=1
fi

echo "*** CHECKING HADDOCK COVERAGE ***"
haddock_failed=0
./ci_scripts/haddock.sh &> /dev/null
if [[ $? -ne 0 ]]; then
  haddock_failed=1
  any_failed=1
fi

echo "*** REPORT ***"
if [[ $cabal_failed -ne 0 ]]; then
  echo "- Cabal format failed"
fi
if [[ $nix_failed -ne 0 ]]; then
  echo "- Nix format failed"
fi
if [[ $ormolu_failed -ne 0 ]]; then
  echo "- Haskell format failed"
fi
if [[ $hlint_failed -ne 0 ]]; then
  echo "- Hlint format failed"
fi
if [[ $haddock_failed -ne 0 ]]; then
  echo "- Haddock format failed"
fi

if [[ $any_failed -ne 0 ]]; then
  exit 1
else
  echo "- All passed"
fi