%%------------------------------------------------------------------------------
%% cs_api: CraftingSoftware coding challenge API module
%%------------------------------------------------------------------------------
-module(cs_api).

-include("cs_api.hrl").

-export([
    process_request/1,
    process_request/2
]).

%%==============================================================================
%% API functions
%%==============================================================================

-spec process_request(cs_req()) -> ok_error_result().
%%------------------------------------------------------------------------------
%% Process request, return response as JSON blob by default
%%------------------------------------------------------------------------------
process_request(Request) ->
    process_request(Request, json).

-spec process_request(cs_req(), cs_req_opt()) -> ok_error_result().
%%------------------------------------------------------------------------------
%% Process request, return response either as JSON blob or as a BASH script
%%------------------------------------------------------------------------------
process_request(Request, OutputFormat) ->
    try
        do_process_request(Request, OutputFormat)
    catch
        throw:{error, _} = Error -> Error;
        _:_ -> {error, <<"internal error">>}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

do_process_request(#{<<"tasks">> := Tasks}, OutputFormat) ->
    Graph = digraph:new([acyclic, private]),
    {[Root | _] = Vertices, ByName} = create_vertices(Graph, Tasks),
    ok = create_edges(Graph, ByName, Vertices),
    {ok, render(Graph, sort(Graph, Root), OutputFormat)};
do_process_request(_Request, _OutputFormat) ->
    {error, <<"request must contain 'tasks' field">>}.

create_vertices(Graph, Tasks) ->
    Root = add_vertex(Graph, [root, undefined, []]),
    {Vertices, ByName} = lists:mapfoldl(
        fun(#{<<"name">> := Name, <<"command">> := Command} = Task, Index) ->
            case maps:get(Name, Index, undefined) of
                undefined ->
                    Deps = maps:get(<<"requires">>, Task, [root]),
                    Vertex = add_vertex(Graph, [Name, Command, Deps]),
                    {Vertex, Index#{Name => Vertex}};
                _ ->
                    abort(<<"duplicate task name: ", Name/binary>>)
            end
        end,
        #{}, Tasks),
    {[Root | Vertices], ByName#{root => Root}}.

create_edges(Graph, ByName, Vertices) ->
    lists:foreach(
        fun(Vertex) ->
            {Vertex, [Name, _Command, Deps]} = digraph:vertex(Graph, Vertex),
            lists:foreach(
                fun(DepName) ->
                    Dep = resolve_dependency(DepName, ByName),
                    case digraph:add_edge(Graph, Vertex, Dep) of
                        {error, {bad_edge, _Path}} ->
                            abort(<<"cyclic dependency found in: ",
                                Name/binary>>);
                        _Edge ->
                            ok
                    end
                end,
                Deps)
        end,
        Vertices).

add_vertex(Graph, Label) ->
    digraph:add_vertex(Graph, digraph:add_vertex(Graph), Label).

resolve_dependency(DepName, ByName) ->
    case maps:get(DepName, ByName, undefined) of
        undefined -> abort(<<"unknown dependency: ", DepName/binary>>);
        Dep -> Dep
    end.

sort(Graph, Root) ->
    [Root | Sorted] = sort(Graph, Root, []),
    remove_duplicates(Sorted).

sort(Graph, Vertex, Result) ->
    [Vertex | lists:foldl(
        fun(Dependant, Acc) -> sort(Graph, Dependant, Acc) end,
        Result, digraph:in_neighbours(Graph, Vertex))].

remove_duplicates(List) ->
    lists:foldl(
        fun(Item, Acc) ->
            case lists:member(Item, Acc) of
                true -> Acc;
                false -> [Item | Acc]
            end
        end,
        [], lists:reverse(List)).

render(Graph, Tasks, OutputFormat) ->
    Output = lists:map(
        fun(Vertex) ->
            {Vertex, [Name, Command, _Deps]} = digraph:vertex(Graph, Vertex),
            #{<<"name">> => Name, <<"command">> => Command}
        end,
        Tasks),
    case OutputFormat of
        json -> iolist_to_binary(json:encode(#{<<"tasks">> => Output}));
        bash -> bash_encode(Output)
    end.

bash_encode(Output) ->
    lists:foldl(
        fun(#{<<"command">> := C}, Acc) -> <<Acc/binary, "\n", C/binary>> end,
        <<"#!/usr/bin/env bash">>, Output).

abort(Message) ->
    throw({error, Message}).

%%==============================================================================
%% Tests
%%==============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(JSON_BASIC, <<"
    {
        \"tasks\": [
            {
                \"name\": \"task-1\",
                \"command\": \"touch /tmp/file1\"
            },
            {
                \"name\": \"task-2\",
                \"command\": \"cat /tmp/file1\",
                \"requires\": [
                    \"task-3\"
                ]
            },
            {
                \"name\": \"task-3\",
                \"command\": \"echo 'Hello World!' > /tmp/file1\",
                \"requires\": [
                    \"task-1\"
                ]
            },
            {
                \"name\": \"task-4\",
                \"command\": \"rm /tmp/file1\",
                \"requires\": [
                    \"task-2\",
                    \"task-3\"
                ]
            }
        ]
    }
">>).

-define(JSON_CYCLE, <<"
    {
        \"tasks\": [
            {
                \"name\": \"task-1\",
                \"command\": \"touch /tmp/file1\",
                \"requires\": [
                    \"task-2\"
                ]
            },
            {
                \"name\": \"task-2\",
                \"command\": \"cat /tmp/file1\",
                \"requires\": [
                    \"task-3\"
                ]
            },
            {
                \"name\": \"task-3\",
                \"command\": \"echo 'Hello World!' > /tmp/file1\",
                \"requires\": [
                    \"task-1\"
                ]
            }
        ]
    }
">>).

-define(JSON_UNKNOWN_NAME, <<"
    {
        \"tasks\": [
            {
                \"name\": \"task-1\",
                \"command\": \"touch /tmp/file1\",
                \"requires\": [
                    \"task-2\"
                ]
            }
        ]
    }
">>).

-define(JSON_DUPE, <<"
    {
        \"tasks\": [
            {
                \"name\": \"task-1\",
                \"command\": \"touch /tmp/file1\"
            },
            {
                \"name\": \"task-1\",
                \"command\": \"cat /tmp/file1\"
            }
        ]
    }
">>).

-define(BASIC_JSON_OUT, <<
    "{"
        "\"tasks\":["
            "{"
                "\"command\":\"touch /tmp/file1\","
                "\"name\":\"task-1\""
            "},"
            "{"
                "\"command\":\"echo 'Hello World!' > /tmp/file1\","
                "\"name\":\"task-3\""
            "},"
            "{"
                "\"command\":\"cat /tmp/file1\","
                "\"name\":\"task-2\""
            "},"
            "{"
                "\"command\":\"rm /tmp/file1\","
                "\"name\":\"task-4\""
            "}"
        "]"
    "}"
>>).

-define(BASIC_BASH_OUT, <<
    "#!/usr/bin/env bash\n"
    "touch /tmp/file1\n"
    "echo 'Hello World!' > /tmp/file1\n"
    "cat /tmp/file1\n"
    "rm /tmp/file1"
>>).

setup() ->
    ok.

teardown(_) ->
    ok.

all_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            {"test basic example - json output", fun test_basic_json/0},
            {"test basic example - bash output", fun test_basic_bash/0},
            {"test cycle", fun test_cycle/0},
            {"test unknown dependency name", fun test_unknown_dep/0},
            {"test duplicate task name", fun test_dupe/0}
        ]
    }.

test_basic_json() ->
    ?assertEqual(
        {ok, ?BASIC_JSON_OUT},
        process_request(json:decode(?JSON_BASIC), json)).

test_basic_bash() ->
    ?assertEqual(
        {ok, ?BASIC_BASH_OUT},
        process_request(json:decode(?JSON_BASIC), bash)).

test_cycle() ->
    ?assertEqual(
        {error, <<"cyclic dependency found in: task-3">>},
        process_request(json:decode(?JSON_CYCLE), bash)).

test_unknown_dep() ->
    ?assertEqual(
        {error, <<"unknown dependency: task-2">>},
        process_request(json:decode(?JSON_UNKNOWN_NAME), bash)).

test_dupe() ->
    ?assertEqual(
        {error, <<"duplicate task name: task-1">>},
        process_request(json:decode(?JSON_DUPE), bash)).

-endif. %% TEST
