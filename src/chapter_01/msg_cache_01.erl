-module(msg_cache).

-export([start/1]).
-export([loop/1]).

-record(state, {
	 name,
	 length = 0,
	 buffer = []
	 }).

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
		{pop, [], From} ->
			From ! {error, empty},
			loop(State);
		{pop, [TopMsg | Msgs], From} ->
			From ! {ok, TopMsg},
			loop(State#state{buffer = Msgs, length = Length - 1});
		_Unsupported ->
			erlang:error(io_libs:format("unsupported msg: ", [_Unsupported]))
	end.

start(BufferName) ->
	Pid = spawn(msg_cache, loop, [#state{name = BufferName}]),
	io:format("Buffer ~s created! Pid = ~p~n", [BufferName, Pid]),
	Pid.

