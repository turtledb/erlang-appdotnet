ERL          ?= erl
APP          := appdotnet
REBAR        := ./rebar

all: compile doc

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

doc:
	@$(REBAR) doc

test: all
	@ERL_FLAGS="-args_file test.args" $(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf doc
	@rm -rf ebin
	@rm -rf deps
	@rm -rf logs

dialyzer: compile
	@dialyzer -Wno_match -Wno_return -c ebin/ | tee test/dialyzer.log

console: compile
	@$(ERL) -pa ebin -pa deps/*/ebin -args_file test.args -boot start_sasl
