-module(utils).
-export([formatted_time/0, remove_newline/1, index/1, setnth/3, flush/0, list_to_bits/1]).

formatted_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  io_lib:format("~p.~p.~p ~p:~p:~p~n", [Day, Month, Year, Hour, Minute, Second]).

remove_newline(String) ->
  string:strip(String, right, $\n).

index(List) ->
  lists:zip(List, lists:seq(0, length(List) - 1)).

%% setnth(Index, List, NewElement) -> List.
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

flush() ->
  receive
    _ -> flush()
  after 0 -> ok
end.

list_to_bits(List) ->
  << <<X:1>> || X <- List >>.
