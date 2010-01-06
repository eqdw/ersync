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
    {ok, Checklist} = checksums(Blocklist),
    send_checksums(Socket, Checklist),
    receive_data(Socket, Blocklist).

receive_filename(Socket,Buf) ->
    case gen_tcp:recv(Socket,1) of
        {ok, B}    ->
            receive_filename(Socket, [B|Buf]);
        {ok, "\n"} ->
            {ok, list_to_binary(reverse(Buf))};
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

check128(Block) -> md4:hash(Block).

    
