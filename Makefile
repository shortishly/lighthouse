REBAR = rebar
DIALYZER = dialyzer
PLT = .sse_dialyzer.plt

.PHONY: all deps clean compile test ct build-plt dialyze

all:	clean compile ct

clean:
	@$(REBAR) clean

squeaky: clean
	@$(REBAR) delete-deps

deps:
	@$(REBAR) get-deps


compile:
	@$(REBAR) compile


test:
	@$(REBAR) skip_deps=true eunit

ct:
	@$(REBAR) ct skip_deps=true


build-plt:
	@$(DIALYZER) --build_plt --output_plt $(PLT) \
		--apps tools kernel stdlib sasl inets crypto public_key ssl \
		-r deps

plt-info:
	@$(DIALYZER) --plt_info --plt $(PLT)

dialyze:
	@$(DIALYZER) --src src \
		--plt $(PLT) \
		--verbose \
		-Werror_handling \
		-Wrace_conditions \
		-Wunmatched_returns \
		-Wunderspecs \
		-Wno_behaviours
