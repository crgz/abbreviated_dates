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
remote_version = $(shell curl --silent 'https://api.github.com/repos/crgz/$(name)/releases/latest' | jq -r .tag_name)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

all: check
check: about test # pack_install execute make check

about:
	@echo $(name) v$(version) -- $(title)
	@if [ v$(version) != $(remote_version) ]; then \
		printf '[Warning!] Version out of synch v$(version)/$(remote_version)\n'; \
	fi

test:
	@script -qc "swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl" /dev/null | tail -n +8

test-plain:
	@swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl 2>&1 /dev/null | tail -n +8

remove:
	@swipl -g "pack_remove($(name))"  -t halt

install:
	@echo "(none)"

install-local:
	@swipl -q -g "pack_install('$(name)',[interactive(false)]),halt(0)" -t 'halt(1)'

deploy:
	@bumpversion patch && git push --quiet ;\
	LOCAL_VERSION=$$(swipl -q -s pack -g 'version(V),writeln(V)' -t halt) ;\
	hub release create -m v$$LOCAL_VERSION v$$LOCAL_VERSION ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(name)/releases/latest' | jq -r .tag_name) ;\
		if [ v$$LOCAL_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 1 ;\
  done ;\
	swipl -q -g "pack_remove($(name)),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'
