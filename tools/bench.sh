set -e

export LANG="C.UTF-8"

cabal bench --benchmark-options \
  '--csv benchmarks/bench.csv --svg benchmarks/bench.svg'