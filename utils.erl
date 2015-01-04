-module(utils).
-export([formatted_time/0, remove_newline/1]).

formatted_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  io_lib:format("~p.~p.~p ~p:~p:~p~n", [Day, Month, Year, Hour, Minute, Second]).

remove_newline(String) ->
  string:strip(String, right, $\n).
