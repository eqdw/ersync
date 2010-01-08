-module(server).
-import(lists, [map/2, zip3/3]).

start() ->
    spawn(fun() -> init() end).

stop(Pid) ->
    Pid ! {term, stopped}.

init() ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    server_wait(Listen).

server_wait(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    sync(Socket),
    gen_tcp:close(Socket),
    server_wait(Listen).
    

sync(Socket) ->
    {ok, Filename} = receive_filename(Socket, []),
    {ok, Blocklist} = blocks(Filename),
    md4_interface:start(),
    {ok, Checklist} = checksums(Blocklist),
    md4_interface:stop(),
    send_checksums(Socket, Checklist),
    {ok, Instrs} = receive_data(Socket, []),
    {ok, Outbuf} = build_file(Instrs, Checklist, []),
    write_file(Filename, Outbuf).


build_file([{Type, Val}|Rest]=Instrs, Checklist, Acc) ->
    case Type of
        ptr ->
            {Data, _, _} = nth(Val, Checklist),
            build_file(Rest, Checklist, [Data|Acc]);
        blk ->
            build_file(Rest, Checklist, [Val|Acc])
    end;
build_file([], _Checklist, Acc) -> {ok, list_to_bin(reverse(Acc))}.

write_file(Outbuf) ->
    file:write_file(Filename, [Outbuf]).


received_data(Socket, Buf) ->
    case gen_tcp:recv(Socket, 5) of
        {ok, "PTR"} ->
            {ok, Ptr} = gen_tcp:recv(Socket, 4),
            received_data(Socket, [{ptr, Ptr}|Buf]);
        {ok, "BLK"} ->
            {ok, Len} = gen_tcp:recv(Socket, 4),
            {ok, <<Data:Len>>} = gen_tcp:recv(Socket, Len),
            received_data(Socket, [{blk, Data}|Buf]);
        {ok, "FIN"} ->
            {ok, reverse(Buf)};
        {error, closed} ->
            {error, closed}
    end.
            

send_checksums(Socket, [{B,R,S}|Rest]) ->
    gen_tcp:send(Socket, R),
    gen_tcp:send(Socket, S),
    send_checksums(Socket, Rest);
send_checksums(Socket,[]) -> ok.

receive_filename(Socket,Buf) ->
    case gen_tcp:recv(Socket,1) of
        {ok, B}    ->
            receive_filename(Socket, [B|Buf]);
        {ok, "\n"} ->
            {ok, reverse(Buf)};
        {error, closed} ->
            {error, closed}
    end.

blocks(Filename) ->
    File = file:read_file(Filename), 
    Blocklist = [ X || <<X:512>> <= File], 
    {ok, Blocklist}.

checksums(Blocklist) ->
    Rolling = lists:map(check32/1, Blocklist),
    Strong  = lists:map(check128/1, Blocklist),
    Checklist = zip3(Blocklist, Rolling, Strong),
    {ok, Checklist}.

check32(Block) ->
    <<Rtn:32>> = helper32(Remaining, 0).

helper32(Remaining, Acc) when Acc >= 65536 ->
    helper32(Remaining, Acc-65536);
helper32(<<H:8,T/binary>>, Acc) -> helper32(T, Acc+H);
helper32([], Acc)    -> Acc.

check128(Block) -> md4_interface:call(Block).

    
