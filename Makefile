REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) update-deps
	@$(REBAR) get-deps

compile:
	$(REBAR) compile

fast:
	$(REBAR) compile skip_deps=true

clean:
	@rm -rf deps
	@$(REBAR) clean

.PHONY: all deps compile clean
