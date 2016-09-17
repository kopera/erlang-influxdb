# erlang-influxdb

[InfluxDB](https://www.influxdata.com/time-series-platform/influxdb/) client library for Erlang.

## Building

    $ rebar3 compile

## Interactive session

The output has been reformatted for readability.

    Erlang/OTP 19 [erts-8.0.2] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]
    
    Eshell V8.0.2  (abort with ^G)
    1> application:ensure_all_started(influxdb).
    {ok, [jsone, influxdb]}
    2> Config = influxdb_config:new(#{host => "localhost", username => "root", password => "root"}).
    #{host => "localhost", password => "root", port => 8086, username => "root"}
    3> influxdb:query(Config, "show databases").
    {ok,[[#{name => <<"databases">>,
            columns => [<<"name">>],
            rows => [{<<"_internal">>}]}]]}
    4> influxdb:query(Config, "create database test").
    ok
    5> influxdb:query(Config, "show databases").
    {ok,[[#{name => <<"databases">>,
            columns => [<<"name">>],
            rows => [{<<"_internal">>}, {<<"test">>}]}]]}
    6> influxdb:write(Config#{database => "test"}, [{"cpu_load_short",
        #{"region" => "af-west", "host" => "server01"},
        #{"value" => 0.64}}]).
    ok
    7> influxdb:write(Config#{database => "test"}, [{"cpu_load_short",
        #{"region" => "af-west", "host" => "server02"},
        #{"value" => 0.67}}]).
    ok
    8> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short").
    {ok,[[#{name => <<"cpu_load_short">>,
            columns => [<<"time">>, <<"host">>, <<"region">>, <<"value">>],
            rows => [
                {1474124935934979502, <<"server01">>, <<"af-west">>, 0.64},
                {1474124961985106195, <<"server02">>, <<"af-west">>, 0.67}]}]]}
    9> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short group by host").
    {ok,[[#{name => <<"cpu_load_short">>,
            tags => #{<<"host">> => <<"server01">>},
            columns => [<<"time">>, <<"region">>, <<"value">>],
            rows => [{1474124935934979502, <<"af-west">>, 0.64}]},
          #{name => <<"cpu_load_short">>,
            tags => #{<<"host">> => <<"server02">>},
            columns => [<<"time">>, <<"region">>, <<"value">>],
            rows => [{1474124961985106195, <<"af-west">>, 0.67}]}]]}
    10> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short group by host; select * from cpu_load_short").
    {ok,[[#{name => <<"cpu_load_short">>,
            tags => #{<<"host">> => <<"server01">>},
            columns => [<<"time">>, <<"region">>, <<"value">>],
            rows => [{1474124935934979502, <<"af-west">>, 0.64}]},
          #{name => <<"cpu_load_short">>,
            tags => #{<<"host">> => <<"server02">>},
            columns => [<<"time">>, <<"region">>, <<"value">>],
            rows => [{1474124961985106195, <<"af-west">>, 0.67}]}],
         [#{name => <<"cpu_load_short">>,
            columns => [<<"time">>, <<"host">>, <<"region">>, <<"value">>],
            rows => [
                {1474124935934979502, <<"server01">>, <<"af-west">>, 0.64},
                {1474124961985106195, <<"server02">>, <<"af-west">>, 0.67}]}]]}
