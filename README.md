cs_api
=====

This is a Proof-Of-Concept implementation. It focuses on the algorithmic task of job orchestration and provides a very basic implementation of the HTTP layer. There's no external dependencies, this POC implementation is based on the standard libraries only.

Build
-----

You need the Erlang runtime **and** rebar3 to be properly installed and added to the PATH

    $ make

Test
-----

    $ make test

Run
-----

This will start the service in Erlang shell. By default the service listens at port 8000. You can modify [erl.config](erl.config) to use a different port number.

    $ make shell

Query
-----

Get response in JSON format (default) using [tasks.json](tasks.json) as the request body:

    $ curl -X PUT http://localhost:8000/cs/cs_api_http:handle -d @tasks.json

Get response in JSON format (explicitly):

    $ curl -X PUT http://localhost:8000/cs/cs_api_http:handle/json -d @tasks.json

Get response as a bash script:

    $ curl -X PUT http://localhost:8000/cs/cs_api_http:handle/bash -d @tasks.json
