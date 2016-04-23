WATCHES=src include $(wildcard rebar.*)

.PHONY: all lint test brahmin_runner _build/default/bin/brahmin_runner

all: lint test brahmin_runner

lint:
	REBAR_PROFILE=dev rebar3 dialyzer
	REBAR_PROFILE=dev rebar3 xref

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write $(WATCHES); \
	done

test:
	rebar3 eunit

brahmin_runner: _build/default/bin/brahmin_runner
	cp $< $@

_build/default/bin/brahmin_runner:
	rebar3 escriptize
