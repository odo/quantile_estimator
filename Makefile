all: deps compile_all

compile_all:
	rebar compile

compile:
	rebar compile skip_deps=true

deps:
	rebar get-deps

clean:
	rebar clean

test:
	TESTDIR=$(PWD)/private/ rebar skip_deps=true eunit

shell:
	erl -pz ebin deps/*/ebin

check: compile
	dialyzer -Wno_undefined_callbacks ebin/*.beam

xref: compile
	rebar xref skip_deps=true