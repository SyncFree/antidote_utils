
-type downstream_record() :: term().
-type key() :: term().
-type op()  :: {update | merge, downstream_record()}.
-type type() :: atom().
-type dcid() :: 'undefined' | {_,_}.
-type snapshot_time() ::  vectorclock:vectorclock().
-type commit_time() ::  {dcid(), non_neg_integer()}.


-record(tx_id, {snapshot_time :: snapshot_time(), 
                server_pid :: pid()}).
-record(clocksi_payload, {key :: key(),
                          type :: type(),
                          op_param :: op(),
                          snapshot_time :: snapshot_time(),
                          commit_time :: commit_time(),
                          txid :: txid()}).

-type vectorclock() :: vectorclock:vectorclock().
-type txid() :: #tx_id{}.
-type clocksi_payload() :: #clocksi_payload{}.
