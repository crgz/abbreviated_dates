# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all about help synchronize test bump release install requirements committer publish diagrams clean remove-all
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

all: help

.PHONY: help  ## Print this help
help: about
	@printf '\n\033[1;36m%-12s\033[0m %s\n────────────────────────\n' "Command" "Description"
	@awk 'BEGIN {FS = " *## |: "}; /^.PHONY: /{printf "\033[1;36m%-12s\033[0m %s\n", $$2, $$3}' $(MAKEFILE_LIST)

.PHONY: about  ## Describe this tool
NAME = $(shell awk -F"[()]" '/name/{print $$2}' pack.pl)
TITLE = $(shell awk -F"[()]" '/title/{print $$2}' pack.pl)
PACK_PATH = ${HOME}/.local/share/swi-prolog/pack
PACKAGE_PATH = /usr/bin

DISTRIBUTION_CODENAME := $(shell awk -F'=' '/UBUNTU_CODENAME/{print $$2}' /etc/os-release)
SUPPORTED_DISTRIBUTIONS := focal jammy
ifeq ($(filter $(DISTRIBUTION_CODENAME),$(SUPPORTED_DISTRIBUTIONS)),)
    $(warning Terminating on detection of unsupported Ubuntu distribution: $(DISTRIBUTION_CODENAME). \
    Supported distibutions are: $(SUPPORTED_DISTRIBUTIONS))
endif

PPA_PATH = /etc/apt/sources.list.d
HUB_PPA := $(shell [ $$(lsb_release -r|cut -f2) = 18.04 ] && echo $(PPA_PATH)/cpick-ubuntu-hub-$(DISTRIBUTION_CODENAME).list || echo "")

all: about

about:
	@: $${VERSION:=$$(swipl -q -s pack -g 'version(V),format("v~a",[V]),halt')} ; echo $(NAME) $$VERSION -- $(TITLE)

help: about ## Print this help
	@printf '\e[1;34m\n%s\e[m\n\n' "List of available commands:"
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[1;36m%-12s\033[0m %s\n", $$1, $$2}'

synchronize: ## Synchronize the local repository: Switch to the main branch, fetch changes & delete merged branches
	@git checkout main && git pull && git branch --merged | egrep -v "(^\*|main)" | xargs -r git branch -d || exit 0

test: requirements  ## Run the test suite
	@swipl -g 'load_test_files([]),run_tests,halt' prolog/$(NAME).pl

bump: $(PACKAGE_PATH)/bumpversion ## Increase the version number
	@bumpversion --allow-dirty --no-commit --no-tag --list patch

# Requires unprotected main branch or maybe special token
release: $(PACKAGE_PATH)/hub ## Release recipe to be use from Github Actions
	@LOCAL_VERSION=$$(awk -F=' ' '/current_version/{printf "v%s",$$2}' .bumpversion.cfg) ;\
	REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
	if [ $$LOCAL_VERSION == $$REMOTE_VERSION ]; then exit; fi ;\
	hub release create -m $$LOCAL_VERSION $$LOCAL_VERSION

install: requirements committer ## Install the latest library release or the one in the VERSION variable (Eg. make install VERSION=v.0.0.207)
	@LOCAL_VERSION=$$(swipl -q -s pack -g 'version(V),format("v~a",[V]),halt') ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
		if [ $$LOCAL_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 4 ;\
	done ;\
	: $${VERSION:=$$LOCAL_VERSION} ;\
	REMOTE=https://github.com/crgz/$(NAME)/archive/$$VERSION.zip ;\
	swipl -qg "pack_remove($(NAME)),pack_install('$$REMOTE',[interactive(false)]),halt(0)" -t 'halt(1)'

requirements: packages packs  ## Install the packages packs required for the development environment
packages: $(PACKAGE_PATH)/swipl $(PACKAGE_PATH)/git
packs: $(PACK_PATH)/tap  $(PACK_PATH)/date_time

committer:
	@git config --global user.email "conrado.rgz@gmail.com" && git config --global user.name "Conrado Rodriguez"

GIT_REPO_URL := $(shell git config --get remote.origin.url)

publish: diagrams ## Publish the diagrams
	@echo $(GIT_REPO_URL) \
	&& cd target/publish \
	&& git init . \
	&& git remote add github ${GIT_REPO_URL} \
	&& git checkout -b gh-pages \
	&& git add . \
	&& git commit -am "Static site deploy" \
	&& git push github gh-pages --force \
	&& cd ../.. || exit

diagrams: workflow

#
#  workflow
#
workflow: target/publish/workflow.svg  ## Creates the Diagrams
target/publish/workflow.svg:
	@printf '\e[1;34m%-6s\e[m\n' "Start generation of scalable C4 Diagrams"
	@mvn exec:java@generate-diagrams -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "Start generation of portable C4 Diagrams"
	@mvn exec:java@generate-diagrams -DoutputType=png -Dlinks=0  -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "The diagrams has been generated"

clean: ## Remove debris
	rm -rfd target

remove-all: ## Remove packages and packs
	@swipl -g "(member(P,[abbreviated_dates,date_time,tap]),pack_property(P,library(P)),pack_remove(P),fail);true,halt"
	@dpkg --purge swi-prolog bumpversion hub
	@add-apt-repository --remove -y ppa:swi-prolog/stable
	@add-apt-repository --remove -y ppa:cpick/hub
	@rm -f $(HUB_PPA) /etc/apt/sources.list.d/swi-prolog-ubuntu-stable-$(DISTRIBUTION_CODENAME).list
	@apt -y autoremove

$(PACKAGE_PATH)/swipl: /etc/apt/sources.list.d/swi-prolog-ubuntu-stable-$(DISTRIBUTION_CODENAME).list
	@apt-get -qqy install swi-prolog-nox
	@touch $@
/etc/apt/sources.list.d/swi-prolog-ubuntu-stable-$(DISTRIBUTION_CODENAME).list:
	apt-add-repository -y ppa:swi-prolog/stable
	@touch $@

$(PACKAGE_PATH)/hub: $(HUB_PPA)
	@apt install -y hub
	@touch $@
/etc/apt/sources.list.d/cpick-ubuntu-hub-$(DISTRIBUTION_CODENAME).list:
	@add-apt-repository -ny ppa:cpick/hub  # Let the last repo do the update
	@touch $@

$(PACKAGE_PATH)/%: # Install packages from default repo
	@apt install $(notdir $@) -y

$(PACK_PATH)/%:
	@swipl -qg "pack_install('$(notdir $@)',[interactive(false)]),halt"
