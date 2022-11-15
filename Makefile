# Care must be taken to not include the Makefile in the pack archive file. If you include the project Makefile in the
# pack, then the pack is treated as a foreign even when it contains no native code. I learned it by the hard way by
# having a Makefile included and seeing all the weird results when make all was run in a location where it was not
# expected. https://rlaanemets.com/post/show/prolog-pack-development-experience

.PHONY: all about help synchronize test bump release install requirements committer publish diagrams clean remove-all
SHELL = /bin/bash
.SHELLFLAGS = -o pipefail -c

NAME = $(shell awk -F"[()]" '/name/{print $$2}' pack.pl)
TITLE = $(shell awk -F"[()]" '/title/{print $$2}' pack.pl)
PACK_PATH = ${HOME}/.local/share/swi-prolog/pack
PACKAGE_PATH = /usr/bin
PPA_PATH = /etc/apt/sources.list.d
HUB_PPA := $(shell [ $$(lsb_release -r|cut -f2) = 18.04 ] && echo $(PPA_PATH)/cpick-ubuntu-hub-bionic.list || echo "")

all: packages

about:
	@: $${VERSION:=$$(swipl -q -s pack -g 'version(V),format("v~a",[V]),halt')} ; echo $(NAME) $$VERSION -- $(TITLE)

help: about ## Print this help
	@printf '\e[1;34m\n%s\e[m\n\n' "List of available commands:"
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[1;36m%-12s\033[0m %s\n", $$1, $$2}'

synchronize: ## Synchronize the local repository: Switch to the main branch, fetch changes & delete merged branches
	@git checkout main && git pull && git branch --merged | egrep -v "(^\*|main)" | xargs -r git branch -d || exit 0

test: requirements  ## Run the test suite
	@swipl -g 'load_test_files([]),run_tests,halt' prolog/epigrapher.pl

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

# Setup requirements
dev: requirements jdk ## Install the packages required for the development environment
requirements: packages packs  ## Install the packages required for the execution environment
packages: $(PPA_PATH)/swi-prolog-ubuntu-stable-bionic.list $(PACKAGE_PATH)/swipl $(PACKAGE_PATH)/git

PID=/tmp/epigrapher.pid
server-start: server-stop pack-install
	@swipl -s prolog/prerequisites.pl -g install,halt && swipl -s prolog/epigrapher.pl -g server
server-stop:
	@if [ -f $(PID) ]; then kill -HUP $$(cat $(PID)) || rm $(PID); fi

pack-install: packs $(SOURCES)
	swipl -qg "pack_remove($(NAME)),halt"
	tar zcvf $(NAME)-$(VERSION).tgz pack.pl prolog/
	swipl -qg "pack_install('$(NAME)-$(VERSION).tgz'),halt"

packs: $(PACK_PATH)/tap  $(PACK_PATH)/date_time

$(PACK_PATH)/%:
	@swipl -qg "pack_install('$(notdir $@)',[interactive(false)]),halt"

jdk: ## Install the packages required for the performance tests
	apt install maven openjdk-11-jdk

remove-all: clean packs-remove ## Remove packages and packs
	@sudo dpkg --purge swi-prolog bumpversion hub
	@sudo add-apt-repository --remove -y ppa:swi-prolog/stable
	@sudo add-apt-repository --remove -y ppa:cpick/hub
	@sudo rm -f $(HUB_PPA) $(PPA_PATH)/swi-prolog-ubuntu-stable-bionic.list
	@sudo apt -y autoremove

clean: ## Remove performance tests results
	mvn clean -f performance/pom.xml

packs-remove: ## Remove packs
	@swipl -g "remove,halt" prolog/prerequisites.pl

$(PPA_PATH)/cpick-ubuntu-hub-bionic.list:
	@sudo add-apt-repository -ny ppa:cpick/hub  # Let the last repo do the update
$(PPA_PATH)/swi-prolog-ubuntu-stable-bionic.list:
	@sudo add-apt-repository -y ppa:swi-prolog/stable

$(PACKAGE_PATH)/swipl:
	@sudo apt install -y swi-prolog
$(PACKAGE_PATH)/hub: $(HUB_PPA)
	@sudo apt install -y hub
$(PACKAGE_PATH)/%: # Install packages from default repo
	@sudo apt install $(notdir $@) -y

# Performance
test-load:
	mvn -e verify -DskipITs=false -Pperformance -f performance/pom.xml
jmeter-run:
	mvn -f performance/pom.xml -Pperformance jmeter:gui -DguiTestFile=src/test/jmeter/test.jmx

PID=/tmp/epigrapher.pid
server-start: server-stop
	@swipl prolog/epigrapher.pl --port=3000 --debug='http(request)' --pidfile=$(PID) --sighup=quit --fork
server-stop:
	@if [ -f $(PID) ]; then kill -HUP $$(cat $(PID)) || rm $(PID); fi

#
# docker
#
.PHONY: docker-run
PORT ?= 3000
docker-run: docker-build  ## Run the application in the Docker container
	docker run -it -p $(PORT):$(PORT) --name="$(NAME)" $(NAME)

.PHONY: docker-build
BASE_IMAGE ?= swipl
docker-build:  ## Build the Docker container for the app
	docker build  --build-arg "PORT=$(PORT)" --build-arg "BASE_IMAGE=$(BASE_IMAGE)" -t $(NAME) .

.PHONY: docker-push
docker-push: docker-build ## push the Docker container for the app
	docker push $(NAME)

.PHONY: docker-clean
docker-clean: ## Remove docker instances
	docker stop $(NAME) || true && docker rm $(NAME) || true

#
#  diagrams
#
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
workflow: target/publish/workflow.svg  ## Creates the Diagrams
target/publish/workflow.svg:
	@printf '\e[1;34m%-6s\e[m\n' "Start generation of scalable C4 Diagrams"
	@mvn exec:java@generate-diagrams -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "Start generation of portable C4 Diagrams"
	@mvn exec:java@generate-diagrams -DoutputType=png -Dlinks=0  -f .github/plantuml/
	@printf '\n\e[1;34m%-6s\e[m\n' "The diagrams has been generated"

committer:
	@git config --global user.email "conrado.rgz@gmail.com" && git config --global user.name "Conrado Rodriguez"