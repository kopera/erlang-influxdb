-module(influxdb_config).
-export([
    new/1
]).
-export_type([
    config/0
]).


-type config() :: #{
    host := string(),
    port := inet:port_number(),
    username := string(),
    password := string(),
    database => string()
}.


-spec new(Opts) -> config() when
    Opts :: #{
        host => string(),
        port => inet:port_number(),
        username => string(),
        password => string(),
        database => string()
    }.
new(Opts) ->
    maps:merge(#{
        host => "localhost",
        port => 8086,
        username => "root",
        password => "root"
    }, Opts).
