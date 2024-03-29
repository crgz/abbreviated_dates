# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

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
about:
	@: $${VERSION:=$$(swipl -q -s pack -g 'version(V),format("v~a",[V]),halt')} ; echo $(NAME) $$VERSION -- $(TITLE)

#
# Superuser rules
#
.PHONY: utilities  ## Install utilities required for the tool (Run with sudo)
DISTRIBUTION_CODENAME := $(shell awk -F'=' '/UBUNTU_CODENAME/{print $$2}' /etc/os-release)
SUPPORTED_DISTRIBUTIONS := focal jammy
ifeq ($(filter $(DISTRIBUTION_CODENAME),$(SUPPORTED_DISTRIBUTIONS)),)
    $(warning Terminating on detection of unsupported Ubuntu distribution: $(DISTRIBUTION_CODENAME). \
    Supported distibutions are: $(SUPPORTED_DISTRIBUTIONS))
endif
PROLOG_LIST_FILE = /etc/apt/sources.list.d/swi-prolog-ubuntu-stable-$(DISTRIBUTION_CODENAME).list
LAST_HUB_LIST_FILE = /etc/apt/sources.list.d/cpick-ubuntu-hub-$(DISTRIBUTION_CODENAME).list
HUB_LIST_FILE := $(shell [ $$(lsb_release -r|cut -f2) = 18.04 ] && echo $(LAST_HUB_LIST_FILE) || echo "")
utilities: packages /usr/bin/swipl

.PHONY: packages  ## Install packages required for the tool (Run with sudo)
packages:
	sudo apt-get update
	apt-get -qqy install git bumpversion hub

/usr/bin/swipl: $(PROLOG_LIST_FILE)
	@apt-get -qqy install swi-prolog-nox
	@touch $@
$(PROLOG_LIST_FILE):
	apt-add-repository -y ppa:swi-prolog/stable
	@touch $@

/usr/bin/hub: $(HUB_LIST_FILE)
	@apt-get install -y hub
	@touch $@
$(HUB_LIST_FILE):
	@add-apt-repository -ny ppa:cpick/hub  # Let the last repo do the update
	@touch $@

ARCH=$(shell dpkg --print-architecture)
GH_KEYRING = /usr/share/keyrings/githubcli-archive-keyring.gpg
GH_LIST = "deb [arch=$(ARCH) signed-by=$(GH_KEYRING)] https://cli.github.com/packages stable main"
GH_LIST_FILE = /etc/apt/sources.list.d/github-cli.list
/usr/bin/gh: $(GH_LIST_FILE)
	@apt-get install $(notdir $@) -y
$(GH_LIST_FILE): $(GH_KEYRING)
	@echo $(GH_LIST) | sudo tee $(GH_LIST_FILE) > /dev/null
	@sudo apt-get update
	@touch $@
$(GH_KEYRING):
	@curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=$(GH_KEYRING)
	@touch $@

/usr/bin/%: # Install packages from default repo
	@apt-get install $(notdir $@) -y

#
# Unprivileged user rules
#
.PHONY: synchronize ## Synchronize the local repository: Switch to the main branch, fetch changes & delete merged branches
synchronize: /usr/bin/git
	@git checkout main && git pull && git branch --merged | egrep -v "(^\*|main)" | xargs -r git branch -d || exit 0

.PHONY: test ## Run the test suite
test: /usr/bin/swipl packs ## Run the test suite
	@swipl -g 'load_test_files([]),run_tests,halt' prolog/$(NAME).pl

.PHONY: store-token ## Store the Github token
store-token:
	secret-tool store --label='github.com/crgz' user ${USER} domain github.com

.PHONY: bump ## Increase the version number
bump: export GH_TOKEN ?= $(shell secret-tool lookup user ${USER} domain github.com) # Overridable
bump: /usr/bin/bumpversion committer
	@git push -d origin release || true
	@git checkout -b release
	@bumpversion --allow-dirty --list patch
	@git push origin release
	@gh pr create -B main -H release --fill
	@gh pr merge -m --auto --delete-branch

.PHONY: release ## Release a new version (Requires unprotected main branch or special token to be used from Github Actions)
release: export GH_TOKEN ?= $(shell secret-tool lookup user ${USER} domain github.com) # Overridable
release: /usr/bin/hub
	@LOCAL_VERSION=$$(awk -F=' ' '/current_version/{printf "v%s",$$2}' .bumpversion.cfg) ;\
	REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
	if [ $$LOCAL_VERSION == $$REMOTE_VERSION ]; then exit; fi ;\
	hub release create -m $$LOCAL_VERSION $$LOCAL_VERSION

.PHONY: install ## Install the latest library release or the one in the VERSION variable (Eg. make install VERSION=v.0.0.207)
install:  /usr/bin/swipl packs committer
	@LOCAL_VERSION=$$(swipl -q -s pack -g 'version(V),format("v~a",[V]),halt') ;\
	while : ; do \
		REMOTE_VERSION=$$(curl --silent 'https://api.github.com/repos/crgz/$(NAME)/releases/latest' | jq -r .tag_name) ;\
		if [ $$LOCAL_VERSION == $$REMOTE_VERSION ]; then printf '\n' && break; fi ;\
		printf '.' && sleep 4 ;\
	done ;\
	: $${VERSION:=$$LOCAL_VERSION} ;\
	REMOTE=https://github.com/crgz/$(NAME)/archive/$$VERSION.zip ;\
	swipl -qg "pack_remove($(NAME)),pack_install('$$REMOTE',[interactive(false)]),halt(0)" -t 'halt(1)'

.PHONY: packs ## Install the required packs
PACK_PATH = ${HOME}/.local/share/swi-prolog/pack
packs: $(PACK_PATH)/tap  $(PACK_PATH)/date_time
$(PACK_PATH)/%:
	@swipl -qg "pack_install('$(notdir $@)',[interactive(false)]),halt"

.PHONY: publish ## Publish the diagrams
GIT_REPO_URL := $(shell git config --get remote.origin.url)
publish: diagrams /usr/bin/git
	@echo $(GIT_REPO_URL) \
	&& cd target/publish \
	&& git init . \
	&& git remote add github ${GIT_REPO_URL} \
	&& git checkout -b gh-pages \
	&& git add . \
	&& git commit -am "Static site deploy" \
	&& git push github gh-pages --force \
	&& cd ../.. || exit

.PHONY: diagrams ## Creates the Diagrams
diagrams: target/publish/workflow.svg
target/publish/workflow.svg:
	@printf '\e[1;34m%-6s\e[m\n' "Start generation of scalable C4 Diagrams"
	@mvn exec:java@generate-diagrams -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "Start generation of portable C4 Diagrams"
	@mvn exec:java@generate-diagrams -DoutputType=png -Dlinks=0  -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "The diagrams has been generated"

.PHONY: clean ## Remove debris from build target
clean:
	rm -rfd target

.PHONY: clean-more ## Remove debris from utilities target
clean-more:
	@swipl -g "(member(P,[abbreviated_dates,date_time,tap]),pack_property(P,library(P)),pack_remove(P),fail);true,halt"
	@dpkg --purge swi-prolog bumpversion hub
	@add-apt-repository --remove -y ppa:swi-prolog/stable
	@add-apt-repository --remove -y ppa:cpick/hub
	@rm -f $(HUB_PPA) /etc/apt/sources.list.d/swi-prolog-ubuntu-stable-$(DISTRIBUTION_CODENAME).list
	@apt-get -y autoremove

.PHONY: committer ## config committer credentials
committer:
	@git config --global user.email "conrado.rgz@gmail.com"
	@git config --global user.name "Conrado Rodriguez"
	@git config pull.ff only
