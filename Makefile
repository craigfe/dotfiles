SHELL := /usr/bin/bash
REGISTRY := r.craigfe.io
DOCKER_IMAGE := $(REGISTRY)/shellcheck
HS_FILES := $(shell git ls-files "*.hs")

SH_FILES := $(shell \
		{ \
			git grep --files-with-matches --extended-regexp -- '^#!.*(bash|[^z]sh)';\
			git ls-files | grep --extended-regexp '.(bash|[^z]sh)$$';\
		} | sort | uniq \
	)

.PHONY: build
build:
	docker build --rm --force-rm -t $(DOCKER_IMAGE) .

.PHONY: lint-sh
lint-sh:
	shellcheck $(SH_FILES)

.PHONY: lint-hs
lint-hs:
	ormolu --check-idempotency --mode check $(HS_FILES)
	hlint $(HS_FILES)

.PHONY: lint
lint: lint-sh lint-hs

.PHONY: format
format:
	ormolu --check-idempotency --mode inplace $(HS_FILES)

.PHONY: test
test: shellcheck

.PHONY: shellcheck
shellcheck:
	docker run --rm -i \
		--name df-shellcheck \
		--volume $(CURDIR):/usr/src:ro \
		--workdir /usr/src \
		r.craigfe.io/shellcheck bash ./test.sh
