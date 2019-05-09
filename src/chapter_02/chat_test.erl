-module(chat_test).

-export([
    test_startup/0,
    test_client_connected/1,
    test_msg_received_from_client/4
]).

-include("chat_protocol.hrl").

test_startup() ->
    route:ensure_db().

test_client_connected(UserID) ->
    chat_server:start_link(UserID, fake_socket).

test_msg_received_from_client(ServerID, FromUserID, ToUserID, Payload) ->
    ServerID ! {tcp, #msg{from_userid = FromUserID, to_userid = ToUserID, payload = Payload}},
    ok.