-module(emqx_auth_kpair_sup).

-behaviour(supervisor).
-define(APP, emqx_auth_kpair).
-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Server} = application:get_env(?APP, server),
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emqx_auth_kpair_dao, Server),
    {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.

