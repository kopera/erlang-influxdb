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
    Args = [{name, {local, influxdb_pool}},
            {worker_module, batch_processor},
            {size, 5}, {max_overflow, 10},
            {batch_proc_fun, BatchProcessFun}],
    PoolSpec = {influxdb_pool, {poolboy, start_link, [Args]},
            permanent, 2000, worker, [poolboy]},
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, [PoolSpec]}}.
