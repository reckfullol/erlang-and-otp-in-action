-record(msg, {
    from_userid,
    to_userid,
    payload
}).

-record(state, {
    userid,
    socket
}).