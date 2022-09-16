# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all about test remove install packs packages deploy
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

NAME = $(shell awk -F"[()]" '/name/{print $$2}' pack.pl)
TITLE = $(shell awk -F"[()]" '/title/{print $$2}' pack.pl)
VERSION = $(shell awk -F"[()]" '/version/{print $$2}' pack.pl)
PACK_PATH ?= ${HOME}/.local/share/swi-prolog/pack
PPA_FILE = /etc/apt/sources.list.d/swi-prolog-ubuntu-stable-bionic.list

all: about packs

about:
	@echo $(NAME) v$(VERSION) -- $(TITLE)

test:
	@swipl -g 'load_test_files([]),run_tests,halt' prolog/$(NAME).pl

remove:
	@swipl -qg "pack_remove($(NAME)),halt"

install: packages $(NAME)

packages: swi-prolog
swi-prolog: swi-prolog-ppa /usr/bin/swipl
swi-prolog-ppa: $(PPA_FILE)
$(PPA_FILE):
	@echo "deb http://ppa.launchpad.net/swi-prolog/stable/ubuntu bionic main" | sudo dd status=none of=$(PPA_FILE)
	@sudo apt update
/usr/bin/swipl:
	sudo apt-get install swi-prolog -y

$(NAME): packs $(PACK_PATH)/$(NAME)
packs: $(PACK_PATH)/tap  $(PACK_PATH)/date_time
$(PACK_PATH)/%:
	@swipl -qg "pack_install('$(notdir $@)',[interactive(false)]),halt"

deploy: packs
	@bumpversion patch && git push --quiet ;\
	NEW_VERSION=$$(swipl -q -s pack -g 'version(V),writeln(V)' -t halt) ;\
	hub release create -m v$$NEW_VERSION v$$NEW_VERSION ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
		if [ v$$NEW_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 1 ;\
	done ;\
	REMOTE=https://github.com/crgz/$(NAME)/archive/v$$NEW_VERSION.zip ;\
	swipl -qg "pack_remove($(NAME)),pack_install('$$REMOTE',[interactive(false)]),halt(0)" -t 'halt(1)'
