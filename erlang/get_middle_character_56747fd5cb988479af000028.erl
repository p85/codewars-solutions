-module(solution).
-export([middle/1]).

middle(String) -> extractString(String).

extractString(S) ->
  Length = string:length(S),
  Position = round(Length / 2),
  case (Length band 1) == 0 of
    true ->
      Offset = 2;
    false ->
      Offset = 1
  end,
  string:substr(S, Position, Offset).