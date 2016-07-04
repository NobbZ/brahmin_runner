WATCHES=src include $(wildcard rebar.*)

SHELL=bash

SCANNER=$(shell find . -name '*.xrl')
PARSER =$(shell find . -name '*.yrl')
GENNED =$(SCANNER:%.xrl=%.erl) $(PARSER:%.yrl=%.erl)

.PHONY: all lint test brahmin_runner _build/default/bin/brahmin_runner dev_brahmin_runner _build/dev/bin/brahmin_runner

all: lint graph test brahmin_runner

graph: modules.svg # applications.svg

modules.svg: dev_brahmin_runner
	./grapherl -m ./_build/dev/lib/brahmin_runner/ $@

applications.svg: dev_brahmin_runner
	./grapherl -a ./_build/dev/lib/brahmin_runner $@

clean:
	rm -rfv _build/{default,dev,test}/lib/brahmin_runner
	rm -rfv _build/{default,dev,test}/bin/brahmin_runner
	rm -rfv $(GENNED)

dist-clean: clean
	rm -rfv _build
	rm -rfv brahmin_runner
	rm -rfv dev_brahmin_runner

lint: dialyze elvis xref

dialyze:
	REBAR_PROFILE=dev rebar3 dialyzer

elvis:
	elvis rock

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

dev_brahmin_runner: _build/dev/bin/brahmin_runner
	cp $< $@

_build/default/bin/brahmin_runner:
	rebar3 escriptize

_build/dev/bin/brahmin_runner:
	REBAR_PROFILE=dev rebar3 escriptize

# This is a target to test the built script!
run:
	@sleep 6
	@echo "[[(0,0,0),(1,1,0)]]"
	@echo "[[(0,0,0),(0,2,1),(2,0,2)]]"
	@sleep 10

TAGS:
	@find . -name '*.[he]rl' ! -path '*/test/*' -print | etags -
