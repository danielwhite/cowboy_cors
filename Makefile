PROJECT = cowboy_cors

DEPS = cowboy
dep_cowboy = https://github.com/extend/cowboy.git 1.0.1

include erlang.mk

.PHONY: test

test:
	CT_SUITES=cors make tests
