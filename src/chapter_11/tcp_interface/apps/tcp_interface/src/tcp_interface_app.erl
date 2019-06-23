%%%-------------------------------------------------------------------
%% @doc tcp_interface public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_interface_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1155).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = case application:get_env(tcp_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case tcp_interface_sup:start_link(LSock) of
        {ok, Pid} ->
            tcp_interface_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
