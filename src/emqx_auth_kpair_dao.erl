-module(emqx_auth_kpair_dao).

-behaviour(ecpool_worker).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([connect/1, query/3]).
%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------

connect(Options) ->
    case mysql:start_link(Options) of
        {ok, Pid} -> {ok, Pid};
        ignore -> {error, ignore};
        {error, Reason = {{_, {error, econnrefused}}, _}} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: Connection refused."),
            {error, Reason};
        {error, Reason = {ErrorCode, _, Error}} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: ~p - ~p", [ErrorCode, Error]),
            {error, Reason};
        {error, Reason} ->
            ?LOG(error, "[MySQL] Can't connect to MySQL server: ~p", [Reason]),
            {error, Reason}
    end.

query(Pool, Sql, Params) ->
    ecpool:with_client(Pool, fun(C) -> mysql:query(C, Sql, Params) end).
