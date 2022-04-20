
name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

SWIPL := swipl

upload: release
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'

release: bump
	@hub release create -m v$(version) v$(version)

push: bump
	@git push

bump: test
	@bumpversion patch

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

version:
	@echo $(version)