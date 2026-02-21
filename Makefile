SHELL := /bin/bash

SH_FILES := $(shell \
		{ \
			git grep --files-with-matches --extended-regexp -- '^#!.*(bash|[^z]sh)';\
			git ls-files | grep --extended-regexp '.(bash|[^z]sh)$$';\
		} | sort | uniq \
	)

STOW_PACKAGES := aerospace aliases claude ghostty git karabiner scripts tmux vim zsh

.PHONY: install
install:
	stow $(STOW_PACKAGES)

.PHONY: uninstall
uninstall:
	stow -D $(STOW_PACKAGES)

.PHONY: lint
lint:
	shellcheck $(SH_FILES)
