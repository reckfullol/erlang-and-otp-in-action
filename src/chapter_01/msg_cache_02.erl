-module(msg_cache_02).

-export([start/1]).
-export([loop/1]).
-export([get_name/1]).
-export([get_length/1]).
-export([pop/1]).
-export([set_name/2]).
-export([push/2]).

-record(state, {
	 name,
	 length = 0,
	 buffer = []
	 }).

-define(API_TIMEOUT, 3000).

call(Pid, Request) ->
	Pid ! Request,
	receive
		Response -> Response
	after ?API_TIMEOUT ->
		{error, api_timeout}
	end.

get_name(CacheID) ->
	call(CacheID, {get_name, self()}).

get_length(CacheID) ->
	call(CacheID, {get_length, self()}).

set_name(CacheID, NewName)->
	call(CacheID, {set_name, NewName, self()}).

pop(CacheID) ->
	call(CacheID, {pop, self()}).

push(CacheID, Msg) ->
	call(CacheID, {push, Msg, self()}).

loop(State = #state{name = Name, length = Length, buffer = Buffer}) ->
	receive
		{get_name, From} ->
			From ! {ok, Name},
			loop(State);
		{get_length, From} ->
			From ! {ok, Length},
			loop(State);
		{set_name, NewName, From} ->
			From ! ok,
			loop(State#state{name = NewName});
		{push, Msg, From} ->
			From ! ok,
			loop(State#state{buffer = [Msg | Buffer], length = Length + 1});
		{pop, From} ->
			case Buffer of
				[] ->
					From ! {error, empty},
					loop(State);
				[TopMsg | Msgs] ->
					From ! {ok, TopMsg},
					loop(State#state{buffer = Msgs, length = Length - 1})
			end;
		{pop, [TopMsg | Msgs], From} ->
			From ! {ok, TopMsg},
			loop(State#state{buffer = Msgs, length = Length - 1});
		_Unsupported ->
			erlang:error(io_libs:format("unsupported msg: ", [_Unsupported]))
	end.

start(BufferName) ->
	Pid = spawn(msg_cache_02, loop, [#state{name = BufferName}]),
	io:format("Buffer ~s created! Pid = ~p~n", [BufferName, Pid]),
	Pid.

