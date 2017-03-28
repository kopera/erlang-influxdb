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
        host => iodata(),
        port => inet:port_number(),
        username => iodata(),
        password => iodata(),
        database => iodata()
    }.
new(Opts) ->
    maps:map(fun
        (host, Host) ->
            unicode:characters_to_list(Host);
        (port, Port) when is_integer(Port), Port > 0, Port < 65536 ->
            Port;
        (username, Username) ->
            unicode:characters_to_list(Username);
        (password, Password) ->
            unicode:characters_to_list(Password);
        (database, Database) ->
            unicode:characters_to_list(Database)
    end, maps:merge(#{
        host => "localhost",
        port => 8086,
        username => "root",
        password => "root"
    }, Opts)).
