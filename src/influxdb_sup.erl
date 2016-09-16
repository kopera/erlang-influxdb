-module(influxdb_sup).
-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @hidden
init([]) ->
    Flags = #{},
    Children = [
    ],
    {ok, {Flags, Children}}.
