REBAR = $(shell pwd)/rebar3 
 
.PHONY: all compile doc clean test dialyzer distclean
 
all: compile test dialyzer
 
# =============================================================================
# Rules to build the system
# =============================================================================
 
compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

doc:
	$(REBAR) edoc
 
eunit: compile
	$(REBAR) eunit
 
test:
	${REBAR} eunit skip_deps=true  

dialyzer:
	${REBAR} dialyzer
  
distclean: clean
	$(REBAR) clean —all
