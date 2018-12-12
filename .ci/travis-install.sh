#!/usr/bin/env bash

set -euo pipefail

: ${GHCVER?}
: ${TRAVIS_OS_NAME?}

travis_retry () {
    $* || (sleep 1 && $*) || (sleep 2 && $*)
}

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  travis_retry sudo apt-get update
  travis_retry sudo apt-get install libleveldb-dev libsnappy-dev
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew install leveldb snappy
else
    echo "Unknown OS: $TRAVIS_OS_NAME"
    exit 1
fi

if ! [ -x $HOME/.ghcup/bin/ghcup ]; then
  mkdir -p $HOME/.ghcup/bin
  cd $HOME/.ghcup/bin
  curl -LO https://github.com/haskell/ghcup/releases/download/0.0.6/ghcup
  curl -LO https://github.com/haskell/ghcup/releases/download/0.0.6/ghcup.asc
  gpg --receive-keys 256844E8AE55008AF197C1B7511B62C09D50CD28
  gpg --verify ghcup.asc ghcup
  chmod +x $HOME/.ghcup/bin/ghcup
fi

export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
ghcup install "$GHCVER"
ghcup set "$GHCVER"
ghcup install-cabal
