% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(fabric2_util).


-export([
    transactional/2,
    transactional/3,

    get_db_handle/0,

    user_ctx_to_json/1,

    uuid/0,

    to_hex/1,

    debug_cluster/0,
    debug_cluster/2
]).


-define(PDICT_DB_KEY, '$erlfdb_handle').


trasactional(Fun) when is_function(Fun, 1) ->
    Db = get_db_handle(),
    erlfdb:transactional(Db, Fun).


get_db_handle() ->
    case get(?PDICT_DB_KEY) of
        undefined ->
            {ok, Db} = application:get_env(fabric, db),
            put(?PDICT_DB_KEY, Db),
            Db;
        Db ->
            Db
    end.


user_ctx_to_json(Db) ->
    UserCtx = fabric2_db:get_user_ctx(Db),
    {[
        {<<"db">>, fabric2_db:name(Db)},
        {<<"name">>, Ctx#user_ctx.name},
        {<<"roles">>, Ctx#user_ctx.roles}
    ]}.



uuid() ->
    to_hex(crypto:strong_rand_bytes(16)).


to_hex(Bin) ->
    list_to_binary(to_hex_int(Bin)).


to_hex_int(<<>>) ->
    [];
to_hex_int(<<Hi:4, Lo:4, Rest/binary>>) ->
    [nibble_to_hex(Hi), nibble_to_hex(Lo) | to_hex(Rest)];


nibble_to_hex(I) ->
    case I of
        0 -> $0;
        1 -> $1;
        2 -> $2;
        3 -> $3;
        4 -> $4;
        5 -> $5;
        6 -> $6;
        7 -> $7;
        8 -> $8;
        9 -> $9;
        10 -> $a;
        11 -> $b;
        12 -> $c;
        13 -> $d;
        14 -> $e;
        15 -> $f
    end.


debug_cluster() ->
    debug_cluster(<<>>, <<16#FE, 16#FF, 16#FF>>).


debug_cluster(Start, End) ->
    transactional(fun(Tx) ->
        lists:foreach(fun({Key, Val}) ->
            io:format("~s => ~s~n", [
                    string:pad(erlfdb_util:repr(Key), 60),
                    erlfdb_util:repr(Val)
                ])
        end, erlfdb:get_range(Tx, Start, End))
    end).