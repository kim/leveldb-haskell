#!/usr/bin/env bash

set -euo pipefail

: ${GHCVER?}
: ${TRAVIS_OS_NAME?}

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then

  travis_retry sudo add-apt-repository -y ppa:hvr/ghc
  travis_retry sudo apt-get update
  travis_retry sudo apt-get install \
      "cabal-install-2.4" \
      "ghc-$GHCVER" \
      libleveldb-dev \
      libsnappy-dev
  export PATH="/opt/ghc/$GHCVER/bin:/opt/cabal/2.4/bin:$PATH"

elif [[ "$TRAVIS_OS_NAME" == "osx" ]]; then

    mkdir -p $HOME/.ghcup/bin
    curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > $HOME/.ghcup/bin/ghcup
    chmod +x $HOME/.ghcup/bin/ghcup
    export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
    ghcup install "$GHCVER"
    ghcup set "$GHCVER"
    ghcup install-cabal
    brew install leveldb snappy

else
    echo "Unknown OS: $TRAVIS_OS_NAME"
    exit 1
fi
