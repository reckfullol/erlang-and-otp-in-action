-module(pingpong).

-export([start/0]).

start() ->
	Pid = spawn(fun ping/0),
	Pid ! self(),
	receive
		pong -> io:format("ok~n")
	end.

ping() ->
	receive
		From -> From ! pong
	end.
