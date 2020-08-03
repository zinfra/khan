SHELL        := /usr/bin/env bash
NAME         := khan
VERSION      ?=
BUILD_NUMBER ?= 0
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
DOCKER       ?= false

OUT_CLI      := dist/$(NAME)
OUT_SYNC     := dist/khan-metadata-sync
OUT          := $(OUT_CLI) $(OUT_SYNC)

default: all

#all: clean install link
all:
	echo $(DEB)

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
ifeq ($(DOCKER),true)
install: init
	docker run --rm \
		-v $(CURDIR):/src \
		mitchty/alpine-ghc:7.10 \
		sh -c 'apk update && apk add build-base xz ncurses zlib-dev ca-certificates && cd /src && stack --system-ghc --local-bin-path=dist --allow-different-user install --flag khan:static'
else
install: init
	stack install --test --local-bin-path=dist
endif

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
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=dist

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" > .metadata

link: bin/khan bin/khan-metadata-sync

bin/khan:
	ln -fs ../$(BIN_CLI) $@

bin/khan-metadata-sync:
	ln -fs ../$(BIN_SYNC) $@
