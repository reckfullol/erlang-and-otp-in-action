%%%-------------------------------------------------------------------
%%% @author reckful
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 15:55
%%%-------------------------------------------------------------------
-module(gws_connection_sup).
-author("reckful").

-behaviour(supervisor).

%% API
-export([
    start_link/4,
    start_child/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

start_child(Server) ->
    supervisor:start_child(Server, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([Callback, IP, Port, UserArgs]) ->
    BasicSockOpts = [
        binary,
        {active, false},
        {packet, http_bin},
        {reuseaddr, true}
    ],
    SockOpts = case IP of
                   undefined -> BasicSockOpts;
                   _ -> [{ip, IP} | BasicSockOpts]
               end,
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = {
        gws_server,
        {gws_server, start_link, [Callback, LSock, UserArgs]},
        temporary,
        brutal_kill,
        worker,
        [gws_server]
        },
    RestartStrategy = {simple_one_for_one, 1000, 3600},
    {ok, {RestartStrategy, [Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
