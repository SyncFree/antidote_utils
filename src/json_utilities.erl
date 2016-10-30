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
-module(json_utilities).
-include("antidote_utils.hrl").

-export([
	 crdt_to_json/2,
	 crdt_from_json/1,
	 txid_to_json/1,
	 txid_from_json/1,
	 pid_to_json/1,
	 pid_from_json/1,
	 dcid_to_json/1,
	 dcid_from_json/1,
	 atom_to_json/1,
	 atom_from_json/1,
	 clocksi_payload_to_json/1,
	 clocksi_payload_from_json/1,
	 convert_to_json/1,
	 deconvert_from_json/1,
	 list_to_json_binary_list/1,
	 json_binary_list_to_list/1,
	 json_binary_to_erlang_term/1,
	 json_binary_to_binary/1,
	 erlang_term_to_json_binary/1,
	 binary_to_json_binary/1
	]).

crdt_to_json(Type, Value) ->
    Type:to_json(Value).

crdt_from_json([{orset,Value}]) ->
    crdt_orset:from_json([{orset,Value}]).

txid_to_json(#tx_id{local_start_time=Time,server_pid=Pid}) ->
    [{txid,[convert_to_json(Time),atom_to_json(Pid)]}].

txid_from_json([{txid,[JTime,JPid]}]) ->
    #tx_id{local_start_time=deconvert_from_json(JTime),
    %%TODO: This seems wrong; PIDs are not atoms.
	  server_pid=atom_from_json(JPid)}.

pid_to_json(PID) ->
    [{pid,PID}].

pid_from_json([{pid,BPID}]) ->
    LPID = 
	case BPID of
	    _ when is_atom(BPID) ->
		atom_to_list(BPID);
	    _ when is_list(BPID) ->
		BPID;
	    _ when is_binary(BPID) ->
		binary_to_list(BPID)
	end,
    list_to_pid(LPID).

dcid_to_json(undefined) ->
    [{dcid, undefined}];
dcid_to_json({Id,{T1,T2,T3}}) ->
    [{dcid, [Id,T1,T2,T3]}];
dcid_to_json(Other) ->
    [{dcid, convert_to_json(Other)}].

dcid_from_json([{dcid, undefined}]) ->
    undefined;
dcid_from_json([{dcid, <<"undefined">>}]) ->
    undefined;
dcid_from_json([{dcid, [JId,T1,T2,T3]}]) ->
    Id = atom_from_json(JId),
    {Id,{T1,T2,T3}};
dcid_from_json([{dcid, Other}]) ->
    deconvert_from_json(Other).

atom_to_json(Type) when is_atom(Type) ->
    Type.

atom_from_json(JType) when is_binary(JType) ->
    binary_to_atom(JType, utf8);
atom_from_json(JType) when is_atom(JType) ->
    JType.

clocksi_payload_to_json(#clocksi_payload{key=Key,type=Type,op_param=Op,snapshot_time=SnapshotTime,commit_time={DCID,CT},txid=TxId}) ->
    JKey = convert_to_json(Key),
    JType = atom_to_json(Type),
    JOp = 
	case Op of
	    {update, Downstream} ->
		[{update, Type:downstream_to_json(Downstream)}];
	    {merge, State} ->
		[{merge, Type:to_json(State)}]
	end,
    JSnapshotTime = vectorclock:to_json(SnapshotTime),
    JCommitTime = [dcid_to_json(DCID),CT],
    JTxId = txid_to_json(TxId),
    [{clocksi_payload,[[{key,JKey}],
		       [{type,JType}],
		       JOp,
		       [{snapshot_time,JSnapshotTime}],
		       [{commit_time,JCommitTime}],
		       JTxId]}].

clocksi_payload_from_json([{clocksi_payload,[[{key,JKey}],
					     [{type,JType}],
					     JOp,
					     [{snapshot_time,JSnapshotTime}],
					     [{commit_time,[JDCID,CT]}],
					     JTxId]}]) ->
    Key = deconvert_from_json(JKey),
    Type = atom_from_json(JType),
    Op = 
	case JOp of
	    [{update, JDownstream}] ->
		{update, Type:downstream_from_json(JDownstream)};
	    [{merge, JState}] ->
		{merge, Type:from_json(JState)}
	end,
    SnapshotTime = vectorclock:from_json(JSnapshotTime),
    CommitTime = {dcid_from_json(JDCID),CT},
    TxId = txid_from_json(JTxId),
    #clocksi_payload{key=Key,type=Type,op_param=Op,snapshot_time=SnapshotTime,commit_time=CommitTime,txid=TxId}.

convert_to_json(Elem) ->
    IsJSON = jsx:is_term(Elem),
    {IsUtf8,IsBinary} =
	case is_binary(Elem) of
	    true ->
		case unicode:characters_to_list(Elem,utf8) of
		    Res when is_list(Res) ->
			{true,true};
		    _ ->
			{false,true}
		end;
	    false ->
		{false,false}
	end,
    case Elem of
	Elem when IsUtf8 ->
	    [{json_value,Elem}];
	Elem when IsBinary ->
	    [{binary64,binary_to_json_binary(Elem)}];
	Elem when IsJSON ->
	    [{json_value,Elem}];
	Elem ->
	    [{erlang_term_binary64,erlang_term_to_json_binary(Elem)}]
    end.

deconvert_from_json([{ObjectType,Object}]) ->
    case ObjectType of
	json_value ->
	    Object;
	erlang_term_binary64 ->
	    json_binary_to_erlang_term(Object);
	binary64 ->
	    json_binary_to_binary(Object)
    end.

list_to_json_binary_list(List) ->
    lists:map(fun(Item) ->
		      convert_to_json(Item)
	      end,List).

json_binary_list_to_list(List) ->
    lists:map(fun(Item) ->
		      deconvert_from_json(Item)
	      end,List).

json_binary_to_erlang_term(Binary) ->
    binary_to_term(json_binary_to_binary(Binary)).
json_binary_to_binary(Binary) ->
    base64:decode(Binary).

erlang_term_to_json_binary(Item) ->
    binary_to_json_binary(term_to_binary(Item)).
binary_to_json_binary(Binary) ->
    base64:encode(Binary).
