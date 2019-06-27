-module(http_interface_server).

-behavior(gen_web_server).

-export([
    start_link/1,
    start_link/2
]).

-export([
    init/1,
    get/3,
    delete/3,
    put/4,
    post/4,
    head/3,
    options/4,
    trace/4,
    other_methods/4
]).

start_link(Port) ->
    gen_web_server:start_link(?MODULE, Port, []).

start_link(IP, Port) ->
    gen_web_server:start_link(?MODULE, IP, Port, []).

init([]) ->
    {ok, []}.

get({http_request, 'GET', {abs_path, <<"/", Key/bytes>>}, _}, _Head, _UserData) ->
    case simple_cache:lookup(Key) of
        {ok, Value} ->
            gen_web_server:http_reply(200, [], Value);
        {error, not_found} ->
            gen_web_server:http_reply(404, "Sorry, no such key.")
    end.

delete({http_request, 'DELETE', {abs_path, <<"/", Key/bytes>>}, _}, _Head, _UserData) ->
    simple_cache:delete(Key),
    gen_web_server:http_reply(200).

put({http_request, 'PUT', {abs_path, <<"/", Key/bytes>>}, _}, head, Body, _UserData) ->
    simple_cache:insert(Key, Body),
    gen_web_server:http_reply(200).

post(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

head(_Request, _Head, _UserData) ->
    gen_web_server:http_reply(501).

options(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

trace(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).

other_methods(_Request, _Head, _Body, _UserData) ->
    gen_web_server:http_reply(501).



