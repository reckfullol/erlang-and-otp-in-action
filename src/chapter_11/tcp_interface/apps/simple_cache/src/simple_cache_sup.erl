%%%-------------------------------------------------------------------
%% @doc simple_cache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_cache_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/2
]).

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
    Element = {simple_cache_element, {simple_cache_element, start_link, []}, temporary, brutal_kill, worker, [simple_cache_element]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
%%====================================================================
%% Internal functions
%%====================================================================


start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).