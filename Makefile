SHELL         := /usr/bin/env bash
NAME          := leveldb-haskell
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox

CONFIGURED    := dist/setup-config
DOCS          := dist/leveldb-haskell-$(VERSION)-docs.tar.gz

HACKAGE       ?= hackage.haskell.org


default: build

.PHONY: build
build: $(CONFIGURED)
	cabal build -j

.PHONY: deps
deps: cabal.sandbox.config
	cabal install -j --only-dep --enable-documentation --enable-test

.PHONY: dist
dist:
	cabal sdist

.PHONY: docs
docs: $(CONFIGURED)
	cabal haddock \
		--hoogle \
		--html \
		--hyperlink-source \
		--haddock-option='--built-in-themes' \
		--haddock-options='-q aliased' \
		--html-location='/package/$$pkg-$$version/docs' \
		--contents-location='/package/$$pkg-$$version'

.PHONY: clean
clean:
	cabal clean

.PHONY: prune
prune: clean
	cabal sandbox delete

.PHONY: publish
publish: upload-package upload-docs

.PHONY: upload-package
upload-package: dist
	cabal upload --username=$(HACKAGE_USER) --password=$(HACKAGE_PASSWORD) \
		dist/leveldb-haskell-$(VERSION).tar.gz

.PHONY: upload-docs
upload-docs: $(DOCS)
	curl -XPUT \
		-H'Content-Type: application/x-tar' \
		-H'Content-Encoding: gzip' \
		--data-binary @$(DOCS) \
		"https://$(HACKAGE_USER):$(HACKAGE_PASSWORD)@$(HACKAGE)/package/leveldb-haskell-$(VERSION)/docs"

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(CABAL_SANDBOX)

$(CONFIGURED): cabal.sandbox.config deps $(NAME).cabal
	cabal configure --enable-test --enable-bench

$(DOCS): docs
	mkdir -p dist/leveldb-haskell-$(VERSION)-docs
	cp -a dist/doc/html/leveldb-haskell/* dist/leveldb-haskell-$(VERSION)-docs
	COPYFILE_DISABLE=1 tar -cvz --format=ustar -f $(DOCS) -C dist leveldb-haskell-$(VERSION)-docs
