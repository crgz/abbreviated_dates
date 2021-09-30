.PHONY: all test clean

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

SWIPL := swipl


version:
	@echo $(version)

all check install: test

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump:
	@bumpversion patch

push:
	@git push

release:
	@hub release create -m v$(version) v$(version)

upload:
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'
