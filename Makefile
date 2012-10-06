ERL          ?= erl
APP          := appdotnet
REBAR        := ./rebar

all: compile docs

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc

test: test-anonymous test-authenticated

test-anonymous: compile
	@$(REBAR) eunit skip_deps=true suites=anonymous_tests

test-authenticated: compile
	@ERL_FLAGS="-args_file test.args" $(REBAR) eunit skip_deps=true suites=authenticated_tests

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf ebin
	@rm -rf deps
	@rm -rf logs

dialyzer: compile
	@dialyzer -Wno_match -Wno_return -c ebin/ | tee test/dialyzer.log

console: compile
	@$(ERL) -pa ebin -pa deps/*/ebin -args_file test.args -boot start_sasl
