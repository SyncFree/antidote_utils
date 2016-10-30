
-type downstream_record() :: term().
-type actor() :: term().
-type key() :: term().
-type op()  :: {update | merge, downstream_record()}.
-type type() :: atom().
-type dcid() :: 'undefined' | {atom(),tuple()}. %% TODO, is this the only structure that is returned by riak_core_ring:cluster_name(Ring)?
-type snapshot_time() ::  vectorclock:vectorclock().
-type clock_time() :: non_neg_integer().
-type dc_and_commit_time() ::  {dcid(), clock_time()}.

-record(tx_id, {local_start_time :: clock_time(), 
                server_pid :: atom()}).
-record(clocksi_payload, {key :: key(),
                          type :: type(),
                          op_param :: op(),
                          snapshot_time :: snapshot_time(),
                          commit_time :: dc_and_commit_time(),
                          txid :: txid()}).

-type vectorclock() :: vectorclock:vectorclock().
-type txid() :: #tx_id{}.
-type clocksi_payload() :: #clocksi_payload{}.
