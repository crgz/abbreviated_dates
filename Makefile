.PHONY: all clean test install uninstall bump push package

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
pack_name = $(shell swipl -q -s pack -g 'name(N),version(V),format("~a-~a.tgz", [N,V])' -t halt)
remote = https://github.com/crgz/$(name)/releases/download/v$(version)/$(pack_name)

SWIPL := swipl

all: test

version:
	@echo $(version)

clean:
ifneq (,$(wildcard $(pack_name)))
	rm $(pack_name)
endif

install:
	@$(SWIPL) -q -g "pack_install(abbreviated_dates,[interactive(false)]),halt(0)" -t 'halt(1)'

uninstall:
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),halt(0)" -t 'halt(1)'

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump: test
	@bumpversion patch

push:
	@git push

package: test
	@tar cvzf $(pack_name) prolog test pack.pl README.md LICENSE

release: test
	@hub release create -m v$(version) v$(version)

submit: bump push package release
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'
