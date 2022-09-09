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

all: about
check: test # pack_install execute make check and install

install:
	@echo "(none)"

about:
	@echo $(name) v$(version) -- $(title)

test:
	@script -qc "swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl" /dev/null | tail -n +8

test-plain:
	@swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl 2>&1 /dev/null | tail -n +8

remove:
	@swipl -g "pack_remove($(name))"  -t halt

install-local: install-dependencies
	@swipl -q -g "pack_install('$(name)',[interactive(false)]),halt(0)" -t 'halt(1)'

install-dependencies:
	@swipl -g "O=[interactive(false)],pack_install(tap,O),pack_install(date_time,O),halt(0)" -t 'halt(1)'

deploy: install-dependencies
	@bumpversion patch && git push --quiet ;\
	NEW_VERSION=$$(swipl -q -s pack -g 'version(V),writeln(V)' -t halt) ;\
	hub release create -m v$$NEW_VERSION v$$NEW_VERSION ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(name)/releases/latest' | jq -r .tag_name) ;\
		if [ v$$NEW_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 1 ;\
  done ;\
  NAME=$$(swipl -q -s pack -g 'name(N),writeln(N)' -t halt) ;\
  REMOTE=https://github.com/crgz/$$NAME/archive/v$$NEW_VERSION.zip ;\
	swipl -g "pack_remove($$NAME),pack_install('$$REMOTE',[interactive(false)]),halt(0)" -t 'halt(1)'
