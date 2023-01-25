# i never thought it would come to this
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

all: clean
	rebar3 get-deps
	rebar3 update-deps
	rebar3 compile
	rebar3 escriptize

clean: clean_doc
	rebar3 clean

test: clean
	rebar3 eunit skip_deps=true

clean_doc:
	rm -rf doc

doc: clean_doc
	rebar3 doc
