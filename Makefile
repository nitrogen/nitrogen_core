.PHONY: test docs doc deps

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

dialyzer-deps-compile:
	./rebar --config "rebar.dialyzer.config" get-deps
	./rebar --config "rebar.dialyzer.config" compile

clean:
	./rebar clean

docs:  
	utils/make_docs/make_docs.el
	@(cd doc;perl add_disqus.pl)

doc: docs

eunit: clean deps compile
	./rebar eunit
	rm -fr deps

test:
	mkdir -p test
	rm -fr test/browsertest
	$(MAKE) eunit
	git clone git://github.com/nitrogen/NitrogenProject.com.git test/browsertest
	mkdir -p test/browsertest/deps
	ln -s ../../.. test/browsertest/deps/nitrogen_core
	cd test/browsertest; make test_all TESTLOGDIR="../results.$(shell date +%Y-%m-%d.%H%M%S)"


DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib crypto sasl
# removed 'sasl' in attempt to minimize memory usage for Travis

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@(dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r ./deps)

dialyzer: dialyzer-deps-compile $(DEPS_PLT)
	@(dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: eunit $(ERLANG_VERSION)

17: dialyzer
18: dialyzer
19: dialyzer
20: dialyzer
21: dialyzer
22: dialyzer
23: dialyzer

vim:
	utils/vim-headers/add_vim.sh
