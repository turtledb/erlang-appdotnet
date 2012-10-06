ERL          ?= erl
APP          := appdotnet
REBAR        := ./rebar

all: compile docs

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc

test: compile
	@ERL_FLAGS="-args_file test.args" $(REBAR) skip_deps=true eunit

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
