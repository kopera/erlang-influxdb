-module(influxdb_http).
-export([
    post/6
]).
-export_type([
    result/0,
    series/0
]).


-spec post(binary(), string(), string(), string(), iodata(), timeout()) ->
      ok
    | {ok, [result()]}
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
-type result() :: [series()].
-type series() :: #{name := binary(), columns := [binary()], rows := [tuple()], tags => #{binary() => binary()}}.
post(Url, Username, Password, ContentType, Body, Timeout) ->
    Authorization = "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password),
    Headers = [{"Authorization", Authorization}],
    case httpc:request(post,
            {binary_to_list(Url), Headers, ContentType, iolist_to_binary(Body)},
            [{timeout, Timeout}],
            [{body_format, binary}],
            influxdb) of
        {ok, {{_, RespCode, _}, RespHeaders, RespBody}} ->
            response(RespCode, RespHeaders, RespBody);
        {error, Reason} ->
            erlang:exit(Reason)
    end.


%% Internals


response(200, _, Body) ->
    case results(jsone:decode(Body)) of
        [] -> ok;
        Results -> {ok, Results}
    end;
response(204, _, _) ->
    ok;
response(400, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    erlang:error({bad_request, unicode:characters_to_list(Message)});
response(404, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    {error, {not_found, unicode:characters_to_list(Message)}};
response(500, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    {error, {server_error, unicode:characters_to_list(Message)}}.


results(#{<<"results">> := Results}) ->
    [series(Series) || #{<<"series">> := Series} <- Results].


series(Series) ->
    [maps:fold(fun
        (<<"name">>, Name, Acc) -> Acc#{name => Name};
        (<<"tags">>, Tags, Acc) -> Acc#{tags => Tags};
        (<<"columns">>, Columns, Acc) -> Acc#{columns => Columns};
        (<<"values">>, Values, Acc) -> Acc#{rows => [list_to_tuple(Value) || Value <- Values]}
    end, #{}, S) || S <- Series].
