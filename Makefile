.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = abbreviated_dates-$(version).tgz
remote = https://github.com/crgz/abbreviated_dates/releases/download/v$(version)/$(packfile)

SWIPL := swipl

all: test

version:
	@echo $(version)

clean:
	rm abbreviated_dates-*.tgz

install:
	@echo "(none)"

install-dev:
	@$(SWIPL) -q -g 'pack_install(tap),halt(0)' -t 'halt(1)'

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump: test
	@bumpversion patch

package: test
	@tar cvzf $(packfile) prolog test pack.pl README.md LICENSE

release: test
	@hub release create -a $(packfile) -m v$(version) v$(version)

submit: clean bump package release
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates), pack_install('$(remote)'),halt(0)" -t 'halt(1)'
