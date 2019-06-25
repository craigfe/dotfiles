REGISTRY := r.craigfe.io
DOCKER_IMAGE := $(REGISTRY)/shellcheck

.PHONY: build
build:
	docker build --rm --force-rm -t $(DOCKER_IMAGE) .

.PHONY: test
test: shellcheck

.PHONY: shellcheck
shellcheck:
	docker run --rm -i \
		--name df-shellcheck \
		--volume $(CURDIR):/usr/src:ro \
		--workdir /usr/src \
		r.craigfe.io/shellcheck bash ./test.sh
