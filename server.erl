-module(server).
-include("config.hrl").
-mode(compile).
-export([start_server/1, open/1, main/1, calculate_offset/1]).

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
  gen_udp:send(Socket, Address, Port, <<File_size:32/integer>>),
  case wait_for_file_map() of
    {ok, File_map} ->
      send_missing_chunks(binary_to_list(File_map));
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

% send_chunk() -> ok.
index(List) ->
  lists:zip(List, lists:seq(0, length(List) - 1)).

calculate_offset({0, Index}) ->
  {true, Index * ?PACKET_SIZE};
calculate_offset({1, _}) ->
  false.

map_to_offsets(Bitmap) ->
  Indexed_map = index(Bitmap),
  lists:filtermap(fun server:calculate_offset/1, Indexed_map).

send_missing_chunks(File_map) ->
  Offsets = map_to_offsets(File_map),
  io:format("Chunks: ~p~n", [Offsets]).

wait_for_file_map() ->
  receive
    {udp, _, _, _, "DONE"} ->
      io:format("Transfer finished!~n"),
      ok;
    {udp, _, _, _, File_map} ->
      {ok, File_map}
  after 10000 ->
      {error, disconnect}
  end.

wait_for_client(Socket) ->
  receive
    {udp, Socket, Address, Port, Filename} ->
      io:format("Download request: ~s~n", [Filename]),
      send_file(Socket, Address, Port, Filename),
      wait_for_client(Socket)
  end.
