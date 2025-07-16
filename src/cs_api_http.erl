%%------------------------------------------------------------------------------
%% cs_api_http: CraftingSoftware coding challenge HTTP API module
%%------------------------------------------------------------------------------
-module(cs_api_http).
-behaviour(mod_esi).
-export([handle/3]).

%%==============================================================================
%% API functions
%%==============================================================================

handle(SessionID, Env, {_, Body}) ->
    Path = proplists:get_value(path_info, Env),
    case get_format(parse_path(Path)) of
        invalid ->
            bad_request(SessionID, "Invalid output format: " ++ Path);
        OutputFormat ->
            try
                Request = json:decode(list_to_binary(Body)),
                case cs_api:process_request(Request, OutputFormat) of
                    {ok, Response} ->
                        case OutputFormat of
                            json -> json_response(SessionID, Response);
                            bash -> text_response(SessionID, Response)
                        end;
                    {error, Error} -> bad_request(SessionID, Error)
                end
            catch
                error:_ ->
                    bad_request(SessionID, "Invalid request format")
            end
    end;
handle(SessionID, _Env, _Input) ->
	bad_request(SessionID, "Invalid request format").

%%==============================================================================
%% Internal functions
%%==============================================================================

parse_path(undefined) -> [];
parse_path(Path)      -> lists:delete([], string:split(Path, "/", all)).

get_format(["bash"]) -> bash;
get_format(["json"]) -> json;
get_format([])       -> json;
get_format(_)        -> invalid.

%% HTTP response building helpers

bad_request(SessionID, Message) ->
    mod_esi:deliver(SessionID, [
        "status: 400 Bad Request\r\n",
        ct_header("text/plain"),
        Message
    ]).

json_response(SessionID, Data) ->
    mod_esi:deliver(SessionID, [ct_header("application/json"), Data]).

text_response(SessionID, Data) ->
    mod_esi:deliver(SessionID, [ct_header("text/plain"), Data]).

ct_header(MimeType) ->
    "Content-type: " ++ MimeType ++ "\r\n\r\n".
