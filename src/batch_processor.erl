%% ----------------------------------------------------------------------------------------
%% @author Hritik Soni <hritik.s@greyorange.sg>
%% @end
%% ----------------------------------------------------------------------------------------

-module(batch_processor).

-behaviour(gen_statem).

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([init/1, callback_mode/0,
         terminate/3]).

-export([ready/3]).

-record(state, {
    batch_proc_fun = undefined,
    flush_threshold
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_statem callbacks
%%%==================================================================

callback_mode() ->
    state_functions.

%% @private
%% @doc Initializes the server
init(Args) ->
    %% Don't store data in main heap to improve performance during congestion
    process_flag(message_queue_data, off_heap),
    BatchProcessFun = proplists:get_value(batch_proc_fun, Args),
    FlushThreshold = proplists:get_value(flush_threshold, Args, 1000000),
    {ok, ready, #state{batch_proc_fun = BatchProcessFun, flush_threshold = FlushThreshold}}.

ready(info, Data, State) ->
    ProcFun = State#state.batch_proc_fun, 
    %% Use erlang:monotonic_time() to avoid time warps
    StartTime = erlang:monotonic_time(millisecond),
    NewData = receive_and_merge([Data], StartTime, State),
    ProcFun(NewData),
    keep_state_and_data.

drop_mq() ->
    receive
        _Data ->
            drop_mq()
    after 0 ->
        ok
    end.

flush_if_necessary(T) ->
    {_, L} = process_info(self(), message_queue_len),
    case L > T of
        true -> drop_mq();
        false -> ok
    end.

receive_and_merge(AccData, _StartTime, State) when length(AccData) >= 500 ->
    flush_if_necessary(State#state.flush_threshold),
    AccData;
receive_and_merge(AccData, StartTime, State) ->
    Time = erlang:monotonic_time(millisecond),
    TimeLeft = 1000 - (Time - StartTime),
    if
        TimeLeft =< 0 ->
            AccData;
        true ->
            receive
                Data ->
                    receive_and_merge([Data | AccData], StartTime, State)
            after
                TimeLeft ->
                    AccData
            end
    end.

%% @private
%% @doc Opposite of init.
terminate(_Reason, _StateName, _State) ->
    ok.
