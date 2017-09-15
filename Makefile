SHELL        := /usr/bin/env bash
NAME         := khan
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb

OUT_CLI      := dist/$(NAME)
OUT_SYNC     := dist/khan-metadata-sync
OUT          := $(OUT_CLI) $(OUT_SYNC)

default: all

all: clean install link

init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean
	-rm -rf dist
	-rm -f .metadata

.PHONY:
compile:
	stack build --test --no-copy-bins

.PHONY: install
install: init
	stack install --test --local-bin-path=dist

.PHONY: dist
dist: install $(DEB) .metadata

$(OUT_CLI): $(BIN_CLI)
	strip -o $(OUT_CLI) $<

$(OUT_SYNC): $(BIN_SYNC)
	strip -o $(OUT_SYNC) $<

$(DEB): $(OUT)
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD_NUMBER) \
	 --architecture=amd64 \
	 --output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

link: bin/khan bin/khan-metadata-sync

bin/khan:
	ln -fs ../$(BIN_CLI) $@

bin/khan-metadata-sync:
	ln -fs ../$(BIN_SYNC) $@
