#!/usr/bin/env bash

set -xeuo pipefail

: ${GHCVER?}
: ${TRAVIS_OS_NAME?}

travis_retry () {
    $* || (sleep 1 && $*) || (sleep 2 && $*)
}

gpg --version

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  travis_retry sudo add-apt-repository -y ppa:hvr/ghc
  travis_retry sudo apt-get update
  travis_retry sudo apt-get install \
      "cabal-install-2.4" \
      "ghc-$GHCVER" \
      libleveldb-dev \
      libsnappy-dev \
      libnuma-dev
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  brew install leveldb snappy
  # bust
  rm -rf $HOME/.ghcup

  if ! [ -x $HOME/.ghcup/bin/ghcup ]; then
    mkdir -p $HOME/.ghcup/bin
    cd $HOME/.ghcup/bin
    travis_retry curl -LO https://github.com/haskell/ghcup/releases/download/0.0.6/ghcup
    travis_retry curl -LO https://github.com/haskell/ghcup/releases/download/0.0.6/ghcup.asc
    travis_retry gpg --keyserver keyserver.ubuntu.com --recv-keys 256844E8AE55008AF197C1B7511B62C09D50CD28
    gpg --verify ghcup.asc ghcup
    chmod +x $HOME/.ghcup/bin/ghcup
  fi

  export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

  ghcup self-update
  ghcup set "$GHCVER" || ghcup --verbose install "$GHCVER" && ghcup set "$GHCVER"
  command -v cabal || ghcup install-cabal

  ls -la "$HOME/.ghcup/bin"
else
    echo "Unknown OS: $TRAVIS_OS_NAME"
    exit 1
fi
