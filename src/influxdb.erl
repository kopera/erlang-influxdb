-module(influxdb).
-export([
    query/2,
    query/3,
    query/4,
    write/2,
    write/5
]).
-export_type([
    config/0,
    point/0,
    measurement/0,
    tags/0,
    fields/0,
    timestamp/0
]).


-type config() :: influxdb_config:config().
-type point() :: influxdb_line_encoding:point().
-type measurement() :: influxdb_line_encoding:measurement().
-type tags() :: influxdb_line_encoding:tags().
-type fields() :: influxdb_line_encoding:fields().
-type timestamp() :: influxdb_line_encoding:timestamp().


-spec query(config(), string()) ->
      ok
    | {ok, jsone:json_object()}
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
query(Config, Query) ->
    query(Config, Query, #{}, infinity).


-spec query(config(), string(), map()) ->
      ok
    | {ok, jsone:json_object()}
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
query(Config, Query, Parameters) ->
    query(Config, Query, Parameters, infinity).


-spec query(config(), string(), map(), timeout()) ->
      ok
    | {ok, jsone:json_object()}
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
query(#{host := Host, port := Port, username := Username, password := Password} = Config, Query, Parameters, Timeout) ->
    Url = influxdb_uri:encode(#{
        scheme => "http",
        host => Host,
        port => Port,
        path => "/query",
        query => case maps:find(database, Config) of
            {ok, Database} -> #{"db" => Database};
            error -> #{}
        end
    }),
    Body = influxdb_uri:encode_query(#{
        q => Query,
        params => jsone:encode(Parameters)
    }),
    influxdb_http:post(Url, Username, Password, "application/x-www-form-urlencoded", Body, Timeout).


-spec write(config(), [point()]) ->
      ok
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
write(#{host := Host, port := Port, username := Username, password := Password, database := Database}, Measurements) ->
    Url = influxdb_uri:encode(#{
        scheme => "http",
        host => Host,
        port => Port,
        path => "/write",
        query => #{"db" => Database}
    }),
    Body = influxdb_line_encoding:encode(Measurements),
    influxdb_http:post(Url, Username, Password, "application/octet-stream", Body, infinity).


-spec write(config(), measurement(), tags(), fields(), timestamp()) ->
      ok
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
write(Config, Measurement, Tags, Fields, Timestamp) ->
    write(Config, [{Measurement, Tags, Fields, Timestamp}]).
