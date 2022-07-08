# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all test bump push release
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

all: test bump push release

test: test-tap
	@swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl 2>&1 /dev/null | tail -n +8

test-visual-friendly: test-tap
	@script -qc "swipl -t 'load_test_files([]), run_tests.' prolog/$(name).pl" /dev/null | tail -n +8

test-tap:
	@swipl -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

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