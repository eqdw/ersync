-module(md4_interface).
-export([start/0, stop/0, call/1]).

start() ->
    spawn(fun() ->
                  register(md4_interface, self()),
                  process_flag(trap_exit, true),
                  Port = open_port({spawn, "./md4driver"}, [{packet, 2}]),
                  loop(Port)
          end).

stop() ->
    md4_interface ! stop.

call(Data) ->
    md4_interface ! {self(), Data},
    receive
        {md4_interface, Result} ->
            list_to_binary(Result)
    end.

loop(Port) ->
    receive
        {Caller, Data} ->
            Port ! {self(), {command, Data}},
            receive
                {Port, {data, Reply}} ->
                    Caller ! {md4_interface, Reply}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason})
    end.
                     
