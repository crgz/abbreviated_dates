.PHONY: all test clean

name = $(shell swipl -q -s pack -g 'name(N),writeln(N)' -t halt)
version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
pack_name = $(shell swipl -q -s pack -g 'name(N),version(V),format("~a-~a.tgz", [N,V])' -t halt)
remote = https://github.com/crgz/$(name)/archive/v$(version).zip

SWIPL := swipl

all: dependencies

version:
	@echo $(version)

check: test

dependencies:
	@$(SWIPL) -q -g "pack_install(tap,[interactive(false)]),halt(0)" -t 'halt(1)'
	@$(SWIPL) -q -g "pack_install(date_time,[interactive(false)]),halt(0)" -t 'halt(1)'

install:
	@echo "(none)"

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump:
	@bumpversion patch

push:
	@git push

package: test
	@tar cvzf $(pack_name) prolog test pack.pl README.md LICENSE

release:
	@hub release create -m v$(version) v$(version)

submit: bump push release
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates),pack_install('$(remote)',[interactive(false)]),halt(0)" -t 'halt(1)'
