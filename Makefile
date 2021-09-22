.PHONY: all test clean

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
pack_name = $(shell swipl -q -s pack -g 'name(N),version(V),format("~a-~a.tgz", [N,V])' -t halt)
remote = https://github.com/crgz/$(name)/releases/download/v$(version)/$(pack_name)

SWIPL := swipl

all: test

version:
	@echo $(version)

clean:
	rm $(name)-*.tgz

install:
	@echo "(none)"

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump: test
	@bumpversion patch

push:
	@git push

package: test
	@tar cvzf $(pack_name) prolog test pack.pl README.md LICENSE

release: test
	@hub release create -a $(pack_name) -m v$(version) v$(version)

submit: bump push package release   
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates), pack_install('$(remote)'),halt(0)" -t 'halt(1)'
