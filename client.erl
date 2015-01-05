-module(client).
-mode(compile).
-include("config.hrl").
-export([main/1, wait_for_command/1]).

main(_) ->
  connect().

connect() ->
  {ok, Socket} = gen_udp:open(0, ?SOCK_OPTS),
  wait_for_command(Socket).

wait_for_command(Socket) ->
  Command = io:get_line(">"),
  handle_command(Command, Socket),
  wait_for_command(Socket).

request_file(Socket, Address, Port, Filename) ->
  gen_udp:send(Socket, Address, Port, Filename),
  receive
    {udp, Socket, _, _, <<File_size:32/integer>>} ->
      {ok, File_size}
  after ?TIMEOUT ->
      {error, timeout}
  end.

download(Socket, Address, Port, Filename) ->
  case request_file(Socket, Address, Port, Filename) of
    {ok, File_size} ->
      io:format("Receiving: ~s - ~B bytes~n", [Filename, File_size]),
      case allocate_file(Filename, File_size, ?PACKET_SIZE) of
        {ok, File, File_map} ->
          ok = gen_udp:send(Socket, Address, Port, File_map),
          receive_file(File, File_map);
        {error, Reason} ->
          io:format("Error: ~p~n", [Reason])
      end;
    {error, timeout} ->
      io:format("Connection error!~n")
  end.

receive_file(File, File_map) ->
  receive
    {udp, Socket, Address, Port, <<"SENT">>} ->
      ok = gen_udp:send(Socket, Address, Port, File_map),
      receive_file(File, File_map);
    {udp, Socket, Address, Port, <<Offset:32/integer, Data/binary>>} ->
      New_file_map = save_chunk(File, Offset, Data, File_map),
      case lists:all(fun (X) -> X =:= 1 end, New_file_map) of
        true ->
          gen_udp:send(Socket, Address, Port, <<"DONE">>),
          utils:flush(),
          file:close(File);
        _ ->
          receive_file(File, New_file_map)
      end
  after ?TIMEOUT ->
    io:format("Timeout!~n")
  end.

allocate_file(_, 0, _) ->
  {error, file_not_found};
allocate_file(Name, Size, Chunk_size) ->
  Chunk_number = trunc(Size/Chunk_size),
  io:format("chunk number: ~B~n", [Chunk_number]),
  File_map = [0 || _ <- lists:seq(0, Chunk_number)],
  Path = "downloads/" ++ Name,
  {ok, File} = file:open(Path, [write]),
  file:allocate(File, 0, Size),
  {ok, File, File_map}.

save_chunk(File, Offset, Data, File_map) ->
  Chunk_index = round(Offset / ?PACKET_SIZE),
  New_file_map = utils:setnth(Chunk_index + 1, File_map, 1),
  file:pwrite(File, Offset, Data),
  New_file_map.

handle_command(Command, Socket) ->
  case Command of
    ("DOWNLOAD " ++ Filename) ->
      download(Socket, ?HOST, ?PORT, utils:remove_newline(Filename));
    _ ->
      io:format("unknown command~n")
  end.
