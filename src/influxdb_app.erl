%% @private
-module(influxdb_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).


-spec start(start_type(), term()) -> {ok, pid()} | {ok, pid(), State :: term()} | {error, term()}.
-type start_type() :: normal | {takeover, node()} | {failover, node()}.
start(_StartType, _StartArgs) ->
    {ok, _} = inets:start(httpc, [
        {profile, influxdb_query},
        {max_sessions, 10},
        {max_keep_alive_length, 0},
        {max_pipeline_length, 0}
    ]),
    {ok, _} = inets:start(httpc, [
        {profile, influxdb_write},
        {max_sessions, 10},
        {max_keep_alive_length, 10},
        {max_pipeline_length, 0}
    ]),
    influxdb_sup:start_link().

-spec(stop(State :: term()) -> term()).
stop(_State) ->
    inets:stop(httpc, influxdb_query),
    inets:stop(httpc, influxdb_write),
    ok.
