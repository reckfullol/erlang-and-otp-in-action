-module(spawn).

-export([start/0]).

start()->
	spawn(io, format, ["Hello World!~n"]).
