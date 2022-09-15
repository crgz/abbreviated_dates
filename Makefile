# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all check about test test-plain remove install deploy
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
title = $(shell swipl -q -s pack -g 'title(V),writeln(V)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
requires = $(shell swipl -q -s pack -g 'requires(V),writeln(V)' -t halt)

all: about test

about:
	@echo $(name) v$(version) -- $(title)

test:
	@script -qc "swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl" /dev/null | tail -n +8

test-plain:
	@swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl 2>&1 /dev/null | tail -n +8

remove:
	@swipl -qg "pack_remove($(name)),halt"

PACK_PATH ?= ${HOME}/.local/share/swi-prolog/pack
install: install-dependencies  $(PACK_PATH)/$(name)
install-dependencies: $(PACK_PATH)/tap  $(PACK_PATH)/date_time

$(PACK_PATH)/%:
	@swipl -q -g "pack_install('$(notdir $@)',[interactive(false)]),halt(0)" -t 'halt(1)'

deploy: install-dependencies remove
	@bumpversion patch && git push --quiet ;\
	NEW_VERSION=$$(swipl -q -s pack -g 'version(V),writeln(V)' -t halt) ;\
	hub release create -m v$$NEW_VERSION v$$NEW_VERSION;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(name)/releases/latest' | jq -r .tag_name) ;\
		if [ v$$NEW_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 1 ;\
  done ;\
  NAME=$$(swipl -q -s pack -g 'name(N),writeln(N)' -t halt) ;\
  REMOTE=https://github.com/crgz/$$NAME/archive/v$$NEW_VERSION.zip ;\
	swipl -qg "pack_install('$$REMOTE',[interactive(false)]),halt"
