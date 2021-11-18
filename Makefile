REBAR?=$(shell which rebar3 || echo ./rebar3)

.PHONY: test

all: compile

clean:
	rm -fr _build rebar.lock

compile:
	$(REBAR) compile

eunit:
	$(REBAR) eunit

publish:
	$(REBAR) hex publish

test:
	mkdir -p test
	rm -fr test/browsertest
	$(MAKE) eunit
	git clone git://github.com/nitrogen/NitrogenProject.com.git test/browsertest
	mkdir -p test/browsertest/deps
	ln -s ../../.. test/browsertest/deps/nitrogen_core
	cd test/browsertest; make test_all TESTLOGDIR="../results.$(shell date +%Y-%m-%d.%H%M%S)"

dash-docs:
	rm -f doc/dash/Nitrogen.tgz
	doc/dash/md2docset
	cd doc/dash; tar --exclude='.DS_Store' -zcvf Nitrogen.tgz Nitrogen.docset

dialyzer:
	$(REBAR) dialyzer

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: eunit dialyzer

vim:
	utils/vim-headers/add_vim.sh


## DON'T THINK WE NEED THIS ANYMORE, BUT WE'LL KEEP IT JUST IN CASE
#ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
#ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

