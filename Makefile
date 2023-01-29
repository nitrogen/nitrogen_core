REBAR?=./rebar3

.PHONY: test

all: compile

clean:
	rm -fr _build rebar.lock

compile: rebar3
	$(REBAR) compile

eunit: rebar3
	rm -fr _build/test
	$(REBAR) eunit --app nitrogen_core

publish: rebar3
	$(REBAR) hex publish

test: rebar3
	mkdir -p test
	rm -fr test/browsertest
	$(MAKE) eunit
	git clone https://github.com/nitrogen/NitrogenProject.com.git -b rebar3 test/browsertest
	mkdir -p test/browsertest/_checkouts
	ln -s ../../.. test/browsertest/_checkouts/nitrogen_core
	cd test/browsertest; make test_all TESTLOGDIR="../results.$(shell date +%Y-%m-%d.%H%M%S)"

dash-docs:
	rm -f doc/dash/Nitrogen.tgz
	doc/dash/md2docset
	cd doc/dash; tar --exclude='.DS_Store' -zcvf Nitrogen.tgz Nitrogen.docset
	mv doc/dash/Nitrogen.tgz .
	@echo "Dash Docset created and can be found at Nitrogen.tgz in this directory"

rebar3:
	@echo "Fetching and compiling updated rebar3 (this will not replace your system-wide rebar3, if you have one)"
	@(cd /tmp && \
	git clone https://github.com/erlang/rebar3 && \
	cd rebar3 && \
	./bootstrap)
	@echo "Installing rebar3 into Nitrogen directory"
	@(mv /tmp/rebar3/rebar3 .)
	@echo "Cleaning up..."
	@(rm -fr /tmp/rebar3)


dialyzer: rebar3
	$(REBAR) dialyzer

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: eunit dialyzer

vim:
	utils/vim-headers/add_vim.sh


## DON'T THINK WE NEED THIS ANYMORE, BUT WE'LL KEEP IT JUST IN CASE
#ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
#ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

