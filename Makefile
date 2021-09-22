.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = abbreviated_dates-$(version).tgz

SWIPL := swipl

all: test

version:
	@echo $(version)

check: test

install:
	@echo "(none)"

install-dev:
	@$(SWIPL) -q -g 'pack_install(tap),halt(0)' -t 'halt(1)'

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump: test
	@bumpversion patch

package: bump
	@tar cvzf $(packfile) prolog test pack.pl README.md LICENSE

release: package
	@hub release create -a $(packfile) -m v$(version) v$(version)
