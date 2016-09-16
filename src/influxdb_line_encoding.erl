-module(influxdb_line_encoding).
-export([
    encode/1,
    encode/4
]).
-export_type([
    point/0,
    measurement/0,
    tags/0,
    fields/0,
    timestamp/0
]).


-type point() :: {measurement(), tags(), fields(), timestamp()} | {measurement(), tags(), fields()}.
-type measurement() :: iodata().
-type tags() :: #{iodata() => iodata()}.
-type fields() :: #{iodata() => number() | boolean() | iodata()}.
-type timestamp() :: integer(). % erlang:system_time().


-spec encode([point()]) -> iodata().
%% @doc encode a list of `Measurement`, `Tags`, `Fields` and `Timestamp` into multiple lines.
encode(Measurements) when is_list(Measurements) ->
    encode_(Measurements, []).

encode_([{Measurement, Tags, Fields, Timestamp} | Rest], Acc) ->
    encode_(Rest, [encode(Measurement, Tags, Fields, Timestamp) | Acc]);
encode_([{Measurement, Tags, Fields} | Rest], Acc) ->
    encode_(Rest, [encode(Measurement, Tags, Fields, undefined) | Acc]);
encode_([], Acc) ->
    lists:reverse(Acc).


-spec encode(measurement(), tags(), fields(), timestamp() | undefined) -> iodata().
%% @doc encode `Measurement`, `Tags`, `Fields` and `Timestamp` into a line, including the final line feed.
encode(Measurement, Tags, Fields, Timestamp) ->
    [encode_measurement(Measurement), encode_tags(Tags), $\s, encode_fields(Fields), encode_timestamp(Timestamp), $\n].


% Internals


%% @doc encode the measurement name, escaping `,` and ` ` (space).
encode_measurement(Measurement) ->
    escape(Measurement, fun
        (C) when C =:= $, orelse C =:= $\s -> [$\\, C];
        (C) -> C
    end).


%% @doc encode the tags map, escaping `,`, ` ` (space) and `=` in both the tag name and tag value. The encoded tags map
%%      includes the `,` prefix in the case of the non empty map.
encode_tags(Tags) ->
    encode_tags(lists:ukeysort(1, [{unicode:characters_to_list(Key), Value} || {Key, Value} <- maps:to_list(Tags)]), []).

encode_tags([], Acc) ->
    lists:reverse(Acc);
encode_tags([{Key, Value} | Rest], Acc) ->
    encode_tags(Rest, [encode_tag_value(Value), $=, encode_key(Key), $, | Acc]).


%% @doc encode the fields map, escaping `,`, ` ` (space) and `=` in the field name and using the right encoding for the
%%      field value based on its type.
encode_fields(Fields) ->
    encode_fields(lists:ukeysort(1, [{unicode:characters_to_list(Key), Value} || {Key, Value} <- maps:to_list(Fields)]), []).

encode_fields([{Key, Value}], Acc) ->
    lists:reverse(Acc, [encode_key(Key), $=, encode_field_value(Value)]);
encode_fields([{Key, Value} | Rest], Acc) ->
    encode_fields(Rest, [$,, encode_field_value(Value), $=, encode_key(Key) | Acc]).


%% @doc encode a timestamp. This includes the prefix ` ` (space) if the value is not undefined.
encode_timestamp(undefined) ->
    [];
encode_timestamp(Timestamp) ->
    [$\s, erlang:integer_to_binary(erlang:convert_time_unit(Timestamp, native, nano_seconds))].


%% @doc encode a tag or field key.
encode_key(Data) ->
    escape(Data, fun
        (C) when C =:= $, orelse C =:= $= orelse C =:= $\s -> [$\\, C];
        (C) -> C
    end).


%% @doc encode a tag value.
encode_tag_value(Data) ->
    escape(Data, fun
        (C) when C =:= $, orelse C =:= $= orelse C =:= $\s -> [$\\, C];
        (C) -> C
    end).


%% @doc encode a field value.
encode_field_value(Value) when is_integer(Value) ->
    [erlang:integer_to_binary(Value), $i];
encode_field_value(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 10}, compact]);
encode_field_value(true) ->
    <<"t">>;
encode_field_value(false) ->
    <<"f">>;
encode_field_value(Value) when is_list(Value) orelse is_binary(Value) ->
    [$", escape(Value, fun
        (C) when C =:= $" -> [$\\, C];
        (C) -> C
    end), $"].

%% @doc encode


%% @doc internal function used for traversing an iodata structure and escaping characters using the given escape
%%      function.
escape(Data, EscapeFun) when is_list(Data) ->
    [if
        is_integer(C) -> EscapeFun(C);
        is_list(C) orelse is_binary(C) -> escape(C, EscapeFun)
    end || C <- Data];
escape(Bin, EscapeFun) when is_binary(Bin) ->
    unicode:characters_to_binary(escape(unicode:characters_to_list(Bin), EscapeFun)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(label(S), unicode:characters_to_list(io_lib:format("~p", [S]))).

encode_measurement_test_() ->
    Tests = #{
        "string" => "string",
        <<"binary">> => "binary",
        ["io", <<"l">>, $i, <<"st">>] => "iolist",
        "spaces🚀️, commas and emoji" => "spaces🚀️\\,\\ commas\\ and\\ emoji",
        "spaces🚀️ and emoji" => "spaces🚀️\\ and\\ emoji"
    },
    [{?label(Measurement), ?_assertEqual(Encoded, unicode:characters_to_list(encode_measurement(Measurement)))}
        || {Measurement, Encoded} <- maps:to_list(Tests)].

encode_tags_test_() ->
    Tests = #{
        #{} => "",
        #{"key" => "value"} => ",key=value",
        #{"b" => "B", ["a", <<"1">>] => "A"} => ",a1=A,b=B"
    },
    [{?label(Tags), ?_assertEqual(Encoded, unicode:characters_to_list(encode_tags(Tags)))}
        || {Tags, Encoded} <- maps:to_list(Tests)].

encode_fields_test_() ->
    Tests = #{
        #{"float" => 1.2} => "float=1.2",
        #{"integer" => 1} => "integer=1i",
        #{"true" => true} => "true=t",
        #{"false" => false} => "false=f",
        #{"string" => "string"} => "string=\"string\""

    },
    [{?label(Fields), ?_assertEqual(Encoded, unicode:characters_to_list(encode_fields(Fields)))}
        || {Fields, Encoded} <- maps:to_list(Tests)].

encode_timestamp_test_() ->
    Now = erlang:system_time(),
    Tests = #{
        undefined => "",
        Now => [$\s, erlang:integer_to_binary(erlang:convert_time_unit(Now, native, nano_seconds))]

    },
    [{?label(Timestamp), ?_assertEqual(Encoded, encode_timestamp(Timestamp))}
        || {Timestamp, Encoded} <- maps:to_list(Tests)].

-endif.
