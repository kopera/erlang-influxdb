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
    BatchProcessFun = influxdb:get_batch_processing_fun(),
    Children = [
        {batch_processor, {batch_processor, start_link, [BatchProcessFun]},
        permanent, infinity, worker, [batch_processor]}
    ],
    RestartStrategy = {one_for_one, 2, 5},
    {ok, {RestartStrategy, Children}}.
