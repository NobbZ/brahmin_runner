WATCHES=src include $(wildcard rebar.*)

SHELL=bash

SCANNER=$(shell find . -name '*.xrl')
PARSER =$(sehll find . -name '*.yrl')
GENNED =$(SCANNER:%.xrl=%.erl) $(PARSER:%.yrl=%.erl)

.PHONY: all lint test brahmin_runner _build/default/bin/brahmin_runner

all: lint test brahmin_runner

clean:
	rm -rfv _build/{default,dev,test}/lib/brahmin_runner
	rm -rfv _build/{default,dev,test}/bin/brahmin_runner
	rm -rfv $(GENNED)

dist-clean: clean
	rm -rfv _build
	rm -rfv brahmin_runner

lint: dialyze xref

dialyze:
	REBAR_PROFILE=dev rebar3 dialyzer

xref:
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
