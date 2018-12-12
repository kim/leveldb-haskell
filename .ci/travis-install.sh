#!/usr/bin/env bash

set -xeuo pipefail

: ${GHCVER?}
: ${TRAVIS_OS_NAME?}

travis_retry () {
    $* || (sleep 1 && $*) || (sleep 2 && $*)
}

gpg --version

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  travis_retry sudo apt-get update
  travis_retry sudo apt-get install libleveldb-dev libsnappy-dev libnuma-dev
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
  gpg --keyserver keys.gnupg.net --recv-keys 256844E8AE55008AF197C1B7511B62C09D50CD28
  gpg --verify ghcup.asc ghcup
  chmod +x $HOME/.ghcup/bin/ghcup
fi

export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
ghcup set "$GHCVER" || ghcup install "$GHCVER"
ghcup set "$GHCVER"
ghcup install-cabal
