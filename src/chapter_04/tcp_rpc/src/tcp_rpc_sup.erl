%%%-------------------------------------------------------------------
%% @doc tcp_rpc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_rpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Server = {tcp_rpc_server, {tcp_rpc_server, start_link, []}, permanent, 2000, worker, [tcp_rpc_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
