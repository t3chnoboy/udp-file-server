-module(server).
-include("config.hrl").
-mode(compile).
-export([start_server/1, open/1, main/1]).

start_server(Port) ->
  spawn(server, open, [Port]).

main(_) ->
  start_server(?PORT),
  timer:sleep(infinity).

open(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  case gen_udp:open(Port, [binary, {active, true}]) of
    {ok, Socket} ->
      wait_for_client(Socket);
    {error, Reason} ->
      io:format("error: ~p~n", [Reason])
  end.

send_file(Socket, Address, Port, Filename) ->
  File_size = filelib:file_size(["uploads/", binary_to_list(Filename)]),
  io:format("~s: ~B ~n", [Filename, File_size]),
  gen_udp:send(Socket, Address, Port, <<File_size:32/integer>>).

send_chunk() -> ok.
send_chunks(File_map) -> ok.

wait_for_file_map() ->
  receive
    {udp, Socket, Address, Port, File_map} ->
      send_chunks(File_map)
  after 10000 ->
      io:format("disconnect...~n")
  end.

wait_for_client(Socket) ->
  receive
    {udp, Socket, Address, Port, Filename} ->
      io:format("Download request: ~s~n", [Filename]),
      send_file(Socket, Address, Port, Filename),
      wait_for_client(Socket)
  end.
