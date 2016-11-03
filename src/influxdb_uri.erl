-module(influxdb_uri).
-export([
    encode/1
]).

-export([
    encode_query/1,
    encode_component/1,
    encode_component_plus/1,
    encode_component/2,
    is_char_reserved/1,
    is_char_unreserved/1
]).
-export_type([
    uri/0,
    query/0
]).


%% == Encoding URI ==

-spec encode(uri()) -> binary().
-type uri() :: #{
    scheme := iodata(),
    authority => iodata(),
    userinfo => iodata(),
    host => iodata(),
    port => non_neg_integer(),
    path := iodata(),
    query => query(),
    fragment => iodata()
}.
%% @doc Encode a URI.
encode(#{scheme := Scheme, path := Path} = Uri) ->
    iolist_to_binary([Scheme, $:, encode_authority(Uri),
        Path,
        case encode_query(maps:get(query, Uri, [])) of
            <<>> -> [];
            EncodedQuery -> [$?, EncodedQuery]
        end,
        case maps:get(fragment, Uri, "") of
            "" -> [];
            Fragment -> [$#, Fragment]
        end]).


-spec encode_authority(uri()) -> iodata().
encode_authority(#{authority := Authority}) ->
    ["//", Authority];
encode_authority(#{host := Host} = Uri) ->
    UserinfoPart = case maps:find(userinfo, Uri) of
        {ok, Userinfo} -> [Userinfo, $@];
        error -> []
    end,
    PortPart = case maps:find(port, Uri) of
        {ok, Port} -> [$:, integer_to_list(Port)];
        error -> []
    end,
    HostPart = case lists:member($:, Host) of
        true -> [$[, Host, $]];
        false -> Host
    end,
    ["//", UserinfoPart, HostPart, PortPart];
encode_authority(_) ->
    <<>>.


%% == Encoding data ==

-spec encode_query(query()) -> binary().
-type query() :: #{query_key() => query_value()} | [{query_key(), query_value()} | query_key()].
-type query_key() :: iodata() | atom().
-type query_value() :: iodata() | boolean().
%% @doc Encode a map or a list of key value pairs into `key1=value1&key2=value2&key3&key4=value4...' where the keys and
%% values are encoded using {@link encode_component_plus/1}.
encode_query(Values) when is_map(Values) ->
    encode_query(maps:to_list(Values));
encode_query(Values) when is_list(Values) ->
    Encoded = lists:map(fun
        ({Key, true}) ->
            encode_component_plus(Key);
        ({_Key, false}) ->
            <<>>;
        ({Key, Value}) ->
            [encode_component_plus(Key), $=, encode_component_plus(Value)];
        (Key) ->
            encode_component_plus(Key)
    end, Values),
    iolist_to_binary(lists:join($&, Encoded)).


-spec encode_component(iodata()) -> binary().
%% @doc Encode a string percent-escaping all but the "unreserved" characters.
encode_component(String) ->
    encode_component(String, fun is_char_unreserved/1).


-spec encode_component(String, Pred) -> binary() when
    String :: atom() | iodata(),
    Pred :: fun((byte()) -> boolean() | binary()).
%% @doc Encode a string percent-escaping based on the given predicate function.
%%
%% The predicate can return either a boolean, or a binary replacement. Returning a boolean determines whether the
%% given byte should be percent escaped or not, returning `true' means that the byte must be left as is, whereas
%% returning false will encode the byte as `%XX' where `XX' is the hex representation of the byte. If a binary is
%% returned, this binary will be used as replacement for the byte.
encode_component(String, Pred) when is_list(String) ->
    encode_component(unicode:characters_to_binary(String), Pred);
encode_component(String, Pred) when is_binary(String) ->
    << <<(case Pred(C) of
        true ->
            <<C>>;
        false ->
            H = encode_hex(C bsr 4),
            L = encode_hex(C band 16#0f),
            <<$%, H, L>>;
        Bin when is_binary(Bin) ->
            Bin
    end)/binary>> || <<C>> <= String >>;
encode_component(String, Pred) when is_atom(String) ->
    encode_component(atom_to_binary(String, utf8), Pred).


-spec encode_component_plus(atom() | iodata()) -> binary().
%% @doc Encode a string percent-escaping all but the "unreserved" characters and converts space characters to `+'.
encode_component_plus(String) ->
    encode_component(String, fun
        ($\s) -> <<$+>>;
        (C) -> is_char_unreserved(C)
    end).


-spec is_char_reserved(byte()) -> boolean().
%% @doc Checks if the character is a URI "reserved" character according to RFC 3986.
%%
%% Reserved characters are specified in <a href="http://tools.ietf.org/html/rfc3986#section-2.2">RFC 3986, section 2.2</a>.
%% The list of characters in the RFC are defined as `:/?#[]@!$&\'()*+,;='.
is_char_reserved(Char) ->
    lists:member(Char, ":/?#[]@!$&\'()*+,;=").


-spec is_char_unreserved(byte()) -> boolean().
%% @doc Checks if the character is a URI "unreserved" character according to RFC 3986.
%%
%% Unreserved characters are specified in <a href="http://tools.ietf.org/html/rfc3986#section-2.3">RFC 3986, section 2.3</a>.
%% The list of characters in the RFC are defined as `0-9' `A-Z' `a-z' as well as `~', `_', `-' and `.'.
is_char_unreserved(Char) ->
    (Char >= $0 andalso Char =< $9) orelse
        (Char >= $A andalso Char =< $Z) orelse
        (Char >= $a andalso Char =< $z) orelse
        lists:member(Char, "~_-.").


encode_hex(0) -> $0;
encode_hex(1) -> $1;
encode_hex(2) -> $2;
encode_hex(3) -> $3;
encode_hex(4) -> $4;
encode_hex(5) -> $5;
encode_hex(6) -> $6;
encode_hex(7) -> $7;
encode_hex(8) -> $8;
encode_hex(9) -> $9;
encode_hex(10) -> $A;
encode_hex(11) -> $B;
encode_hex(12) -> $C;
encode_hex(13) -> $D;
encode_hex(14) -> $E;
encode_hex(15) -> $F.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(label(S), unicode:characters_to_list(io_lib:format("~p", [S]))).

encode_component_test_() ->
    Tests = #{
        "" => <<>>,
        "test" => <<"test">>,
        "hello world" => <<"hello%20world">>,
        "hello world+" => <<"hello%20world%2B">>,
        "hello world♥" => <<"hello%20world%E2%99%A5">>,
        <<0, 1, 255>> => <<"%00%01%FF">>,
        [<<0>>, $\s] => <<"%00%20">>
    },
    [{?label(Input), ?_assertEqual(Output, encode_component(Input))} || {Input, Output} <- maps:to_list(Tests)].

encode_component_plus_test_() ->
    Tests = #{
        "" => <<>>,
        "test" => <<"test">>,
        "hello world" => <<"hello+world">>,
        "hello world+" => <<"hello+world%2B">>,
        "hello world♥" => <<"hello+world%E2%99%A5">>,
        <<0, 1, 255>> => <<"%00%01%FF">>,
        [<<0>>, $\s] => <<"%00+">>
    },
    [{?label(Input), ?_assertEqual(Output, encode_component_plus(Input))} || {Input, Output} <- maps:to_list(Tests)].

-endif.
