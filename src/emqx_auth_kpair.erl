-module(emqx_auth_kpair).
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("emqx/include/types.hrl").

-export([on_client_authenticate/3]).

on_client_authenticate(#{clientid := ClientId,
                         username := Username,
                         password := Password} = _ClientInfo,
                        AuthResult,
                        #{auth_query := AuthQuery, pool := Pool} = _Env) ->
    Success = case emqx_auth_kpair_dao:query(Pool, AuthQuery, [ClientId]) of
        {ok, _Columns, [[_ID, _CID, _PUBK, TOKEN, PRIVK, _IP]]} ->
            Sum = sum_key(binary_to_list(Username), binary_to_list(PRIVK), binary_to_list(ClientId)),
            case (Sum =:= binary_to_list(TOKEN)) orelse (binary_to_list(TOKEN) =:= binary_to_list(Password)) of
                true -> {ok, success};
                _ -> {error, not_authorized}
            end;
        {ok, _Columns, []} ->
            {error, not_authorized};
        {error, Reason} ->
            ?LOG(error, "[MySQL] MySQL query '~p' failed: ~p", [AuthQuery, Reason]),
            {error, not_authorized}
    end,
    case Success of
        {ok, success} ->
            {stop, AuthResult#{auth_result => success}};
        {error, not_authorized} ->
            ?LOG(error, "[emqx_auth_kpair] Client auth failed: '~p'", [{clientId, ClientId, username, Username}]),
            {stop, AuthResult#{auth_result => not_authorized}}
    end.

sum_key(PubK, PrivK, ClientId) ->
    md5(PubK ++ PrivK ++ ClientId).

md5(S) ->
    Md5_bin =  erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].


hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).