-ifndef(cs_api_hrl).
-define(cs_api_hrl, 1).

-type ok_tuple() :: {ok, term()}.
-type error_tuple() :: {error, term()}.
-type ok_error_result() :: ok_tuple() | error_tuple().

%% Representation of a single task within request
-type cs_task() :: #{string() := string() | list(string())}.

%% Representation of the request
-type cs_req() :: #{string() := list(cs_task())}.

%% Request options
-type cs_req_opt() :: json | bash.

-endif. %% cs_api_hrl
