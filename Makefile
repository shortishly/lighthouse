REBAR = rebar
DIALYZER = dialyzer

.PHONY: clean compile test ct build-plt dialyze

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile


test:
	@$(REBAR) skip_deps=true eunit

ct:
	@$(REBAR) ct skip_deps=true


build-plt:
	@$(DIALYZER) --build_plt --output_plt .impel_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .impel_dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns -Wunderspecs -Wno_behaviours
