# i never thought it would come to this
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all compile escriptize test test-ct local-test local-eunit local-ct clean clean_doc doc docker-build docker-test

REBAR3 ?= rebar3
DOCKER_IMAGE ?= magicbeam-test:latest

all: compile escriptize

compile:
	$(REBAR3) compile

escriptize: compile
	$(REBAR3) escriptize

test: docker-test

docker-build:
	docker build -t $(DOCKER_IMAGE) -f Dockerfile.test .

docker-test: docker-build
	docker run -t --rm -v $(ROOT_DIR):/app -w /app $(DOCKER_IMAGE) make local-test

test-ct: docker-build
	docker run -t --rm -v $(ROOT_DIR):/app -w /app $(DOCKER_IMAGE) make local-ct

local-test: local-eunit local-ct escriptize

local-eunit:
	$(REBAR3) eunit

local-ct:
	$(REBAR3) ct

clean: clean_doc
	$(REBAR3) clean

clean_doc:
	rm -rf doc

doc: clean_doc
	$(REBAR3) doc
