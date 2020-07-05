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
    batch_proc_fun = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link(BatchProcessFun) ->
    gen_statem:start_link({local, influx_batch_processor}, ?MODULE, [BatchProcessFun], []).

%%%===================================================================
%%% gen_statem callbacks
%%%==================================================================

callback_mode() ->
    state_functions.

%% @private
%% @doc Initializes the server
init([BatchProcessFun]) ->
    process_flag(message_queue_data, off_heap),
    {ok, ready, #state{batch_proc_fun = BatchProcessFun}}.

ready(info, Data, #state{batch_proc_fun = ProcFun}) ->
    StartTime = os:timestamp(),
    NewData = receive_and_merge([Data], StartTime),
    ProcFun(NewData),
    keep_state_and_data.

receive_and_merge(AccData, _StartTime) when length(AccData) >= 500 ->
    AccData;
receive_and_merge(AccData, StartTime) ->
    Time = os:timestamp(),
    TimeLeft = 1000 - timer:now_diff(Time, StartTime) div 1000, %% ms
    if
        TimeLeft =< 0 ->
            AccData;
        true ->
            receive
                {'$gen_cast', {send_message, _, _, Data}} ->
                    receive_and_merge([Data | AccData], StartTime)
            after
                TimeLeft ->
                    AccData
            end
    end.

%% @private
%% @doc Opposite of init.
terminate(_Reason, _StateName, _State) ->
    ok.
