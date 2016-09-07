
-type downstream_record() :: term().
-type actor() :: term().
-type key() :: term().
-type op()  :: {update | merge, downstream_record()}.
-type type() :: atom().
-type dcid() :: 'undefined' | {atom(),tuple()}. %% TODO, is this the only structure that is returned by riak_core_ring:cluster_name(Ring)?
-type payload() :: term().
-record(operation, {op_number, payload :: payload()}).
-type operation() :: #operation{}.
-type snapshot() :: term().
-type snapshot_time() ::  vectorclock:vectorclock().
-type clock_time() :: non_neg_integer().
-type dc_and_commit_time() ::  {dcid(), clock_time()}.

-record(tx_id, {local_start_time :: clock_time(),
                server_pid :: pid()}).
-record(clocksi_payload, {key :: key(),
                          type :: type(),
                          op_param :: op(),
                          snapshot_time :: snapshot_time(),
                          commit_time :: dc_and_commit_time(),
                          txid :: txid()}).

-type vectorclock() :: vectorclock:vectorclock().
-type txid() :: #tx_id{}.
-type clocksi_payload() :: #clocksi_payload{}.



%% AntidoteDB
-type bucket() :: term().
-record (payload, {key:: key(), type :: type(), op_param, actor :: actor()}).

-record(commit_log_payload, {commit_time :: dc_and_commit_time(),
    snapshot_time :: snapshot_time()
}).

-record(update_log_payload, {key :: key(),
    bucket :: bucket(),
    type :: type(),
    op :: op()
}).

-record(abort_log_payload, {}).

-record(prepare_log_payload, {prepare_time :: non_neg_integer()}).

-type any_log_payload() :: #update_log_payload{} | #commit_log_payload{} | #abort_log_payload{} | #prepare_log_payload{}.

-record(log_operation, {
    tx_id :: txid(),
    op_type :: update | prepare | commit | abort | noop,
    log_payload :: #commit_log_payload{}| #update_log_payload{} | #abort_log_payload{} | #prepare_log_payload{}}).
-record(op_number, {node :: {node(),dcid()}, global :: non_neg_integer(), local :: non_neg_integer()}).

%% The way records are stored in the log.
-record(log_record,{
    version :: non_neg_integer(), %% The version of the log record, for backwards compatability
    op_number :: #op_number{},
    bucket_op_number :: #op_number{},
    log_operation :: #log_operation{}}).
