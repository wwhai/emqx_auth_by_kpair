-module(emqx_auth_kpair_app).

-behaviour(application).

-emqx_plugin(?MODULE).
-define(APP, emqx_auth_kpair).
-define(AUTH, "SELECT `id`,`client_id`,`public_key`,`token`,`private_key`,`ip` FROM `emqx_auth`.`device` WHERE client_id = ?").
-define(ACL, "SELECT allow, ipaddr, username, client_id, access, topic FROM acl WHERE client_id = ?").
-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_kpair_sup:start_link(),
    emqx:hook('client.authenticate', fun emqx_auth_kpair:on_client_authenticate/3, [#{auth_query => ?AUTH, pool =>?APP}]),
    emqx:hook('client.check_acl', fun emqx_acl_by_kpair:on_client_check_acl/5, [#{acl_query => ?ACL, pool =>?APP}]),
    {ok, Sup}.

stop(_State) ->
    emqx:unhook('client.authenticate', fun emqx_auth_kpair:on_client_authenticate/3),
    emqx:unhook('client.check_acl', fun emqx_acl_by_kpair:on_client_check_acl/5).
