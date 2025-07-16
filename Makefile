all: api

api:
	rebar3 compile

test:
	rebar3 eunit

clean:
	rebar3 clean

shell:
	rebar3 shell
