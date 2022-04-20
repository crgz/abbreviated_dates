.PHONY: upload release push test version

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

SWIPL := swipl

upload: 
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'

release: 
	@hub release create -m v$(version) v$(version)

bump: 
	@bumpversion patch

push:
	@git push

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

version:
	@echo $(version)