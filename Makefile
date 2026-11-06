# i never thought it would come to this
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all compile escriptize test test-ct local-test local-eunit local-ct clean clean_doc doc docker-build docker-test dev-upgrade

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

# Unlock before resolving because rebar3 upgrade can leave git branch
# dependencies pinned to stale refs while still exiting successfully.
# `unlock --all` is required, not optional: rebar3 REJECTS the bare
# `rebar3 unlock` form, so dropping the flag means nothing un-pins and
# the bump resolves against the refs already in rebar.lock.
# verify-erlang-lock then reports which refs actually MOVED, against the
# baseline snapshotted before the container runs -- a missing dep is
# fatal, an unchanged ref is not (a dep already at its tip correctly
# does not move).
dev-upgrade: docker-build
	@mkdir -p _build
	@sh scripts/verify-erlang-lock --snapshot > _build/rebar-lock.before
	@target='$(DEP)'; \
	if [ -z "$$target" ]; then \
		echo "Bumping all git deps to latest allowed refs..."; \
		unlock_cmd='rebar3 unlock --all'; \
		rm_cmd='rm -rf _build/default/lib _build/test/lib'; \
	else \
		echo "Bumping $$target..."; \
		unlock_cmd="rebar3 unlock $$target"; \
		rm_cmd="rm -rf _build/default/lib/$$target _build/test/lib/$$target"; \
	fi; \
	docker run -t --rm -v $(ROOT_DIR):/app -w /app $(DOCKER_IMAGE) \
		sh -c "$$unlock_cmd && $$rm_cmd && rebar3 deps && rebar3 lock"
	@rc=0; sh scripts/verify-erlang-lock --since _build/rebar-lock.before $(DEP) || rc=$$?; \
		rm -f _build/rebar-lock.before; \
		exit $$rc
