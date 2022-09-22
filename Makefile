# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all about test install infrastructure packs repositories packages deploy
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

NAME = $(shell awk -F"[()]" '/name/{print $$2}' pack.pl)
TITLE = $(shell awk -F"[()]" '/title/{print $$2}' pack.pl)
VERSION = $(shell awk -F"[()]" '/version/{print $$2}' pack.pl)
PACK_PATH = ${HOME}/.local/share/swi-prolog/pack
PACKAGE_PATH = /usr/bin
PPA_PATH = /etc/apt/sources.list.d
HUB_PPA := $(shell [ $$(lsb_release -r|cut -f2) = 18.04 ] && echo $(PPA_PATH)/cpick-ubuntu-hub-bionic.list || echo "")
REPOS = $(PPA_PATH)/swi-prolog-ubuntu-stable-bionic.list $(HUB_PPA)

all: about

about:
	@echo $(NAME) v$(VERSION) -- $(TITLE) $(current_dir)

deploy: test setup-git
	@git diff --quiet || (echo 'Exiting operation on dirty repo' && exit );\
	bumpversion patch && git push --quiet ;\
	NEW_VERSION=$$(swipl -q -s pack -g 'version(V),writeln(V)' -t halt) ;\
	hub release create -m v$$NEW_VERSION v$$NEW_VERSION ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
		if [ v$$NEW_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 1 ;\
	done ;\
	REMOTE=https://github.com/crgz/$(NAME)/archive/v$$NEW_VERSION.zip ;\
	swipl -qg "pack_remove($(NAME)),pack_install('$$REMOTE',[interactive(false)]),halt(0)" -t 'halt(1)'

test: install
	@swipl -g 'load_test_files([]),run_tests,halt' prolog/$(NAME).pl

install: infrastructure packs
infrastructure: repositories packages

repositories: $(REPOS)
$(PPA_PATH)/cpick-ubuntu-hub-bionic.list:
	@sudo add-apt-repository -ny ppa:cpick/hub  # Let the last repo do the update
$(PPA_PATH)/swi-prolog-ubuntu-stable-bionic.list:
	@sudo add-apt-repository -y ppa:swi-prolog/stable

packages: $(PACKAGE_PATH)/swipl $(PACKAGE_PATH)/bumpversion $(PACKAGE_PATH)/hub
$(PACKAGE_PATH)/swipl:
	@sudo apt install -y swi-prolog
$(PACKAGE_PATH)/%: # Install packages from default repo
	@sudo apt install $(notdir $@) -y

packs: $(PACK_PATH)/tap  $(PACK_PATH)/date_time
$(PACK_PATH)/%:
	@swipl -qg "pack_install('$(notdir $@)',[interactive(false)]),halt"

setup-git:
	@git config --global user.email "conrado.rgz@gmail.com"
	@git config --global user.name "Conrado Rodriguez"

remove-all:
	@swipl -g "(member(P,[abbreviated_dates,date_time,tap]),pack_property(P,library(P)),pack_remove(P),fail);true,halt"
	@sudo dpkg --purge swi-prolog bumpversion hub
	@sudo add-apt-repository --remove -y ppa:swi-prolog/stable
	@sudo add-apt-repository --remove -y ppa:cpick/hub
	@sudo rm -f $(REPOS)
	@sudo apt -y autoremove
