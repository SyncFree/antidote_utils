%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(vectorclock).

-include("antidote_utils.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    get_clock_of_dc/2,
    set_clock_of_dc/3,
%%    create_commit_vector_clock/3,
    from_list/1,
    from_dict/1,
    to_dict/1,
    size/1,
    new/0,
    eq/2,
    lt/2,
    gt/2,
    le/2,
    ge/2,
    find/2,
    strict_ge/2,
    strict_le/2,
    max/1,
    min/1,
    fold/3,
    store/3,
    fetch/2,
    erase/2,
    map/2,
    max_vc/2,
    %%    merge/3,
    min_vc/2, to_json/1, from_json/1]).

-export_type([vectorclock/0]).

-spec new() -> vectorclock().
new() ->
    dict:new().

to_dict(VC) -> VC.
%%    dict:from_list(VC).

from_dict(Dict) -> Dict.
%%    from_list(dict:to_list(Dict)).

size(VC) ->
    dict:size(VC).

fold(F, Acc, [{Key,Val}|D]) ->
    dict:fold(F, F(Key, Val, Acc), D);
fold(F, Acc, []) when is_function(F, 3) -> Acc.

find(Key, Orddict) ->
    dict:find(Key, Orddict).

store(Key, Value, Orddict1) ->
    dict:store(Key, Value, Orddict1).

fetch(Key, Orddict)->
    dict:fetch(Key, Orddict).

erase(Key, Orddict1)->
    dict:erase(Key, Orddict1).

map(Fun, Orddict1) ->
    dict:map(Fun, Orddict1).

-spec get_clock_of_dc(any(), vectorclock()) -> non_neg_integer().
get_clock_of_dc(Key, VectorClock) ->
    case dict:find(Key, VectorClock) of
        {ok, Value} -> Value;
        error -> 0
    end.

-spec set_clock_of_dc(any(), non_neg_integer(), vectorclock()) -> vectorclock().
set_clock_of_dc(Key, Value, VectorClock) ->
    case is_atom(VectorClock) of
        true -> dict:store(Key, Value, new());
        false -> dict:store(Key, Value, VectorClock)
    end.

%%-spec create_commit_vector_clock(any(), non_neg_integer(), vectorclock()) -> vectorclock().
%%create_commit_vector_clock(Key, Value, VectorClock)->
%%    set_clock_of_dc(Key, Value, VectorClock).

-spec from_list([{any(), non_neg_integer()}]) -> vectorclock().
from_list(List) ->
    dict:from_list(List).

-spec max([vectorclock()]) -> vectorclock().
max([]) -> new();
max([V]) -> V;
max([V1,V2|T]) -> max([merge(fun erlang:max/2, V1, V2)|T]).

-spec max_vc(vectorclock(), vectorclock()) -> vectorclock().
max_vc(V, V)-> V;
max_vc(V, ignore)-> V;
max_vc(V, undefined)-> V;
max_vc(ignore, V)-> V;
max_vc(undefined, V)-> V;
max_vc([], V)-> V;
max_vc(V, [])-> V;
max_vc(V1, V2) ->
    NewVC = new(),
    case V1 of
        NewVC -> V2;
        _->
            case V2 of
                NewVC ->V1;
                _-> case ge(V1, V2) of
                    true ->
                        V1;
                    false ->
                        V2
                end
            end
    end.


-spec min_vc(vectorclock(), vectorclock()) -> vectorclock().
min_vc(V, ignore)-> V;
min_vc(ignore, V)-> V;
min_vc(V1, V2) ->
    NewVC = new(),
    
    case V1 of
        NewVC -> V2;
        _->
            case V2 of
                NewVC ->V1;
                _-> case ge(V1, V2) of
                    true ->
                        V2;
                    false ->
                        V1
                end
            end
    end.

-spec min([vectorclock()]) -> vectorclock().
min([]) -> new();
min([V]) -> V;
min([V1,V2|T]) -> min([merge(fun erlang:min/2, V1, V2)|T]).

-spec merge(fun((non_neg_integer(), non_neg_integer()) -> non_neg_integer()), vectorclock(), vectorclock()) -> vectorclock().
merge(F, V1, V2) ->
    AllDCs = dict:fetch_keys(V1) ++ dict:fetch_keys(V2),
    Func = fun(DC) ->
        A = get_clock_of_dc(DC, V1),
        B = get_clock_of_dc(DC, V2),
        {DC, F(A, B)}
    end,
    from_list(lists:map(Func, AllDCs)).

-spec for_all_keys(fun((non_neg_integer(), non_neg_integer()) -> boolean()), vectorclock(), vectorclock()) -> boolean().
for_all_keys(F, V1, V2) ->
    %% We could but do not care about duplicate DC keys - finding duplicates is not worth the effort
    AllDCs = dict:fetch_keys(V1) ++ dict:fetch_keys(V2),
    Func = fun(DC) ->
        A = get_clock_of_dc(DC, V1),
        B = get_clock_of_dc(DC, V2),
        F(A, B)
    end,
    lists:all(Func, AllDCs).

-spec eq(vectorclock(), vectorclock()) -> boolean().
eq(V1, V2) -> for_all_keys(fun(A, B) -> A == B end, V1, V2).

-spec le(vectorclock(), vectorclock()) -> boolean().
le(V1, V2) -> for_all_keys(fun(A, B) -> A =< B end, V1, V2).

-spec ge(vectorclock(), vectorclock()) -> boolean().
ge(V1, V2) -> for_all_keys(fun(A, B) -> A >= B end, V1, V2).

-spec lt(vectorclock(), vectorclock()) -> boolean().
lt(V1, V2) -> for_all_keys(fun(A, B) -> A < B end, V1, V2).

-spec gt(vectorclock(), vectorclock()) -> boolean().
gt(V1, V2) -> for_all_keys(fun(A, B) -> A > B end, V1, V2).

-spec strict_ge(vectorclock(), vectorclock()) -> boolean().
strict_ge(V1,V2) -> ge(V1,V2) and (not eq(V1,V2)).

-spec strict_le(vectorclock(), vectorclock()) -> boolean().
strict_le(V1,V2) -> le(V1,V2) and (not eq(V1,V2)).

to_json(VectorClock) ->
    Elements =
        dict:fold(fun(DCID,Time,Acc) ->
            Acc++[[{dcid_and_time,
                [json_utilities:dcid_to_json(DCID),Time]}]]
        end,[],VectorClock),
    [{vectorclock,Elements}].

from_json([{vectorclock,Elements}]) ->
    List =
        lists:map(fun([{dcid_and_time,[JSONDCID,Time]}]) ->
            {json_utilities:dcid_from_json(JSONDCID),Time}
        end,Elements),
    from_list(List).

-ifdef(TEST).

vectorclock_test() ->
    V1 = vectorclock:from_list([{1,5},{2,4},{3,5},{4,6}]),
    V2 = vectorclock:from_list([{1,4}, {2,3}, {3,4},{4,5}]),
    V3 = vectorclock:from_list([{1,5}, {2,4}, {3,4},{4,5}]),
    V4 = vectorclock:from_list([{1,6},{2,3},{3,1},{4,7}]),
    V5 = vectorclock:from_list([{1,6},{2,7}]),
    ?assertEqual(gt(V1,V2), true),
    ?assertEqual(lt(V2,V1), true),
    ?assertEqual(gt(V1,V3), false),
    ?assertEqual(strict_ge(V1,V3), true),
    ?assertEqual(strict_ge(V1,V1), false),
    ?assertEqual(ge(V1,V4), false),
    ?assertEqual(le(V1,V4), false),
    ?assertEqual(eq(V1,V4), false),
    ?assertEqual(ge(V1,V5), false).

vectorclock_max_test() ->
    V1 = vectorclock:from_list([{1, 5}, {2, 4}]),
    V2 = vectorclock:from_list([{1, 6}, {2, 3}]),
    V3 = vectorclock:from_list([{1, 3}, {3, 2}]),
    
    Expected12 = vectorclock:from_list([{1, 6}, {2, 4}]),
    Expected23 = vectorclock:from_list([{1, 6}, {2, 3}, {3, 2}]),
    Expected13 = vectorclock:from_list([{1, 5}, {2, 4}, {3, 2}]),
    Expected123 = vectorclock:from_list([{1, 6}, {2, 4}, {3, 2}]),
    Unexpected123 = vectorclock:from_list([{1, 5}, {2, 5}, {3, 5}]),
    
    ?assertEqual(eq(max([V1, V2]), Expected12), true),
    ?assertEqual(eq(max([V2, V3]), Expected23), true),
    ?assertEqual(eq(max([V1, V3]), Expected13), true),
    ?assertEqual(eq(max([V1, V2, V3]), Expected123), true),
    ?assertEqual(eq(max([V1, V2, V3]), Unexpected123), false).


vectorclock_min_test() ->
    V1 = vectorclock:from_list([{1, 5}, {2, 4}]),
    V2 = vectorclock:from_list([{1, 6}, {2, 3}]),
    V3 = vectorclock:from_list([{1, 3}, {3, 2}]),
    
    Expected12 = vectorclock:from_list([{1, 5}, {2, 3}]),
    Expected23 = vectorclock:from_list([{1, 3}]),
    Expected13 = vectorclock:from_list([{1, 3}]),
    Expected123 = vectorclock:from_list([{1, 3}]),
    Unexpected123 = vectorclock:from_list([{1, 3}, {2, 3}, {3, 2}]),
    
    ?assertEqual(eq(min([V1, V2]), Expected12), true),
    ?assertEqual(eq(min([V2, V3]), Expected23), true),
    ?assertEqual(eq(min([V1, V3]), Expected13), true),
    ?assertEqual(eq(min([V1, V2, V3]), Expected123), true),
    ?assertEqual(eq(min([V1, V2, V3]), Unexpected123), false).

-endif.
