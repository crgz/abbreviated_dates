name := abbreviated_dates

version = $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

define get_version
    $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
endef

define get_pack_name
    $(shell swipl -q -s pack -g 'version(V),format("$(name)-~a.tgz", [V])' -t halt)
endef

.PHONY: all test clean

remote = https://github.com/crgz/abbreviated_dates/releases/download/v$(call get_version)/$(call get_pack_name)

SWIPL := swipl

all: test

version:
	@echo $(version)

get_pack_name:
	@echo $(call get_pack_name)

clean:
	rm $(name)-*.tgz

install:
	@echo "(none)"

test:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

bump: test
	@bumpversion patch

package: test
	@tar cvzf $(call get_pack_name) prolog test pack.pl README.md LICENSE

release: test
	@hub release create -a $(call get_pack_name) -m v$(call get_version) v$(call get_version)

submit: clean bump package release
	@$(SWIPL) -q -g "pack_remove(abbreviated_dates), pack_install('$(remote)'),halt(0)" -t 'halt(1)'
