# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all test bump push release
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
title = $(shell swipl -q -s pack -g 'title(V),writeln(V)' -t halt)
version = v$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
remote_version = $(shell curl --silent 'https://api.github.com/repos/crgz/$(name)/releases/latest' | jq -r .tag_name)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

all: about test

about:
	@echo $(name) $(version)/$(remote_version) -- $(title)
	@if [ "$version" == "$remote_version" ]; then printf '\e[1;34m[ Ok ]\e[m'; else printf '\e[1;34m[ Fail ]\e[m'; fi

test:
	@swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl 2>&1 /dev/null | tail -n +8

test-visual-friendly:
	@script -qc "swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl" /dev/null | tail -n +8

bump:
	@bumpversion patch

push:
	@git push

release:
	@hub release create -m v$(version) v$(version)

upload:
	@swipl -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'

version:
	@echo $(version)