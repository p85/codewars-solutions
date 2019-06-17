-module(mini_string_fuck).
-export([my_first_interpreter/1]).

my_first_interpreter(Code) ->
  parseString(Code, 0, "").

processCharacter(MemoryCell, Character) when Character == "+" ->
  add(MemoryCell);
processCharacter(MemoryCell, _) ->
  MemoryCell.

add(MemoryCell) when MemoryCell > 254 ->
  0;
add(MemoryCell) when MemoryCell < 255 ->
  MemoryCell + 1.

parseString([], _, Result) ->
  StringValue = io_lib:format("~s", [Result]),
  lists:concat(StringValue);
parseString([H | T], MemoryCell, Result) ->
  Character = [H],
  NewMemoryCell = processCharacter(MemoryCell, Character),
  if
    Character == "." ->
      NewResult = Result ++ [NewMemoryCell],
      parseString(T, NewMemoryCell, NewResult);
    true ->
      parseString(T, NewMemoryCell, Result)
  end.