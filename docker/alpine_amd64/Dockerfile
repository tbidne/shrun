# build and test
FROM amd64/alpine:3.21 AS build-stage
LABEL stage=build-stage

ARG CABAL_PROJ
ARG CABAL_VERS
ARG GHC_VERS
ARG suffix

RUN mkdir -p /opt/shrun
ADD . /opt/shrun

RUN <<EOF
set -e

# Taken from haskell/ghcup-hs's dockerfile
apk add --no-cache \
  curl \
  g++ \
  gcc \
  git \
  binutils \
  binutils-gold \
  coreutils \
  bsd-compat-headers \
  gmp \
  gmp-dev \
  libffi-dev \
  ncurses-dev \
  make \
  perl \
  tar \
  xz \
  zlib \
  zlib-dev \
  zlib-static

echo "*** Installing ghcup ***"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh

echo 'PATH=$PATH:$HOME/.ghcup/bin' >> $HOME/.bashrc

. $HOME/.bashrc

echo "*** Installing ghc $GHC_VERS ***"
ghcup install ghc $GHC_VERS --set

echo "*** Installing cabal $CABAL_VERS ***"
ghcup install cabal $CABAL_VERS --set

echo "*** Building shrun ***"

cd /opt/shrun

arch=$(uname -m)

cabal update
cabal install --installdir bin/ --program-suffix $suffix --enable-executable-static --enable-executable-stripping exe:shrun --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Copy binary ***"

mkdir -p /opt/shrun/bin

cd ../../

EOF

# export executable
FROM scratch AS export-stage
LABEL stage=export-stage
COPY --from=build-stage /opt/shrun/bin/* /
