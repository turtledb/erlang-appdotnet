ERL      ?= erl
APP      := appdotnet
VSN      := $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/appdotnet.app.src)
REBAR    := ./rebar
DIALYZER ?= dialyzer

all: compile docs

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf ebin
	@rm -rf deps

console: compile
	@$(ERL) -pa ebin -pa deps/*/ebin -args_file test.args -boot start_sasl

# EUnit

test: test-anonymous test-authenticated

test-anonymous: compile
	@$(REBAR) eunit skip_deps=true suites=anonymous_tests

test-authenticated: compile
	@ERL_FLAGS="-args_file test.args" $(REBAR) eunit skip_deps=true suites=authenticated_tests

# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(APP).plt --apps kernel stdlib sasl inets crypto public_key ssl deps/*

dialyze:
	@$(DIALYZER) src test --src --no_check_plt --plt .$(APP).plt --no_native -Werror_handling -Wunmatched_returns