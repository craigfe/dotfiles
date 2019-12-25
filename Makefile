REGISTRY := r.craigfe.io
DOCKER_IMAGE := $(REGISTRY)/shellcheck
HS_FILES := $(shell git ls-files "*.hs")

.PHONY: build
build:
	docker build --rm --force-rm -t $(DOCKER_IMAGE) .

.PHONY: lint
lint:
	ormolu --check-idempotency --mode check $(HS_FILES)
	hlint $(HS_FILES)

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
