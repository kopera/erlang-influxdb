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
    {ok, [
        #{
            columns => [<<"name">>],
            name => <<"databases">>,
            rows => [{<<"_internal">>}]
        }
    ]}
    4> influxdb:query(Config, "create database test").
    ok
    5> influxdb:query(Config, "show databases").
    {ok, [
        #{
            columns => [<<"name">>],
            name => <<"databases">>,
            rows => [{<<"_internal">>}, {<<"test">>}]
        }
    ]}
    6> influxdb:write(Config#{database => "test"}, "cpu_load_short",
        #{"region" => "af-west", "host" => "server01"},
        #{"value" => 0.64},
        erlang:system_time()).
    ok
    7> influxdb:write(Config#{database => "test"}, "cpu_load_short",
        #{"region" => "af-west", "host" => "server02"},
        #{"value" => 0.67},
        erlang:system_time()).
    ok
    8> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short").
    {ok, [
        [
            #{
                columns => [<<"time">>, <<"host">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [
                    {<<"2016-09-16T16:43:46.524753779Z">>, <<"server01">>, <<"af-west">>, 0.64},
                    {<<"2016-09-16T16:43:59.420361481Z">>, <<"server02">>, <<"af-west">>, 0.67}
                ]
            }
        ]
    ]}
    9> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short group by host").
    {ok, [
        [
            #{
                columns => [<<"time">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [{<<"2016-09-16T16:43:46.524753779Z">>, <<"af-west">>, 0.64}],
                tags => #{<<"host">> => <<"server01">>}
            },
            #{
                columns => [<<"time">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [{<<"2016-09-16T16:43:59.420361481Z">>, <<"af-west">>, 0.64}],
                tags => #{<<"host">> => <<"server02">>}
            }
        ]
    ]}
    10> influxdb:query(Config#{database => "test"}, "select * from cpu_load_short group by host; select * from cpu_load_short").
    {ok, [
        [
            #{
                columns => [<<"time">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [{<<"2016-09-16T16:43:46.524753779Z">>, <<"af-west">>, 0.64}],
                tags => #{<<"host">> => <<"server01">>}
            },
            #{
                columns => [<<"time">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [{<<"2016-09-16T16:43:59.420361481Z">>, <<"af-west">>, 0.64}],
                tags => #{<<"host">> => <<"server02">>}
            }
        ],
        [
            #{
                columns => [<<"time">>, <<"host">>, <<"region">>, <<"value">>],
                name => <<"cpu_load_short">>,
                rows => [
                    {<<"2016-09-16T16:43:46.524753779Z">>, <<"server01">>, <<"af-west">>, 0.64},
                    {<<"2016-09-16T16:43:59.420361481Z">>, <<"server02">>, <<"af-west">>, 0.64}
                ]
            }
        ]
    ]}
