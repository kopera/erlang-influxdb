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
    AppPools = application:get_env(influxdb, app_pools, #{}),
    ExtraPoolSpec =
        maps:fold(
            fun(App, AppSpec, SpecAcc) ->
                case maps:get(influxdb_pool, AppSpec, undefined) of
                    undefined -> SpecAcc;
                    SizeSpec ->
                        DbSpecs =
                        maps:fold(
                            fun(Db, DbSpec, DbSpecAcc) ->
                                PoolName = list_to_atom(atom_to_list(App) ++ "_" ++ Db ++ "_influxdb_pool"),
                                PoolArgs = [{name, {local, PoolName}},
                                        {worker_module, batch_processor},
                                        {size, 5}, {max_overflow, 10},
                                        {batch_proc_fun, BatchProcessFun}] ++ DbSpec,
                                [{list_to_atom(atom_to_list(App) ++ "_" ++ Db ++ "_influxdb_pool"),
                                    {poolboy, start_link, [PoolArgs]}, permanent, 2000, worker, [poolboy]} | DbSpecAcc]

                            end,
                            [],
                            SizeSpec
                        ),
                        DbSpecs ++ SpecAcc
                end
            end,
            [],
            AppPools
        ),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, [PoolSpec | ExtraPoolSpec]}}.
