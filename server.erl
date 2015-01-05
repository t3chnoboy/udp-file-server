-module(server).
-include("config.hrl").
-mode(compile).
-export([start_server/1, open/1, main/1, calculate_offset/1]).

start_server(Ports) when is_list(Ports) ->
  [spawn(server, open, [Port]) || Port <- Ports];
start_server(Port) ->
  spawn(server, open, [Port]).

main(_) ->
  start_server(?PORT),
  timer:sleep(infinity).

open(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  case gen_udp:open(Port, ?SOCK_OPTS) of
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
      send_missing_chunks(Socket, Address, Port, binary_to_list(File_map), binary_to_list(Filename));
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

calculate_offset({0, Index}) ->
  {true, Index * ?PACKET_SIZE};
calculate_offset({1, _}) ->
  false.

map_to_offsets(Bitmap) ->
  Indexed_map = utils:index(Bitmap),
  lists:filtermap(fun server:calculate_offset/1, Indexed_map).

send_missing_chunks(Socket, Address, Port, File_map, Filename) ->
  Offsets = map_to_offsets(File_map),
  File_path = ["uploads/", Filename],
  {ok, File} = file:open(File_path, [read, binary]),
  lists:foreach(fun (Offset) ->
                    {ok, Data} = file:pread(File, Offset, ?PACKET_SIZE),
                    ok = gen_udp:send(Socket, Address, Port, <<Offset:32/integer, Data/binary>>)
                end, Offsets),
  ok = gen_udp:send(Socket, Address, Port, <<"SENT">>),
  case wait_for_file_map() of
    {ok, New_file_map} ->
      send_missing_chunks(Socket, Address, Port, binary_to_list(New_file_map), Filename);
    ok -> ok;
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

wait_for_file_map() ->
  receive
    {udp, _, _, _, <<"DONE">>} ->
      io:format("Transfer finished!~n"),
      ok;
    {udp, _, _, _, File_map} ->
      {ok, File_map}
  after ?TIMEOUT ->
      {error, disconnect}
  end.

wait_for_client(Socket) ->
  receive
    {udp, Socket, Address, Port, Filename} ->
      io:format("Download request: ~s~n", [Filename]),
      send_file(Socket, Address, Port, Filename),
      wait_for_client(Socket)
  end.
