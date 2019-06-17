-module(custom_small_fuck).
-export([interpreter/2]).

interpreter(Code, Tape) ->
  process(Code, Tape, 1, 1, 0).

process(Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
  CodeLength = string:length(Code),
  TapeLength = string:length(Tape),
  IsOutOfBounds = checkOutOfBounds(InstructionPointer, CodeLength, TapePosition, TapeLength),
  case
    IsOutOfBounds of
      out_of_bounds -> Tape;
      ok ->
        Command = getSubString(Code, InstructionPointer),
        processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter)
  end.

processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when Command == ">" ->
  NewTapePosition = TapePosition + 1,
  processFinished(Code, Tape, NewTapePosition, InstructionPointer, LoopCounter);
processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when Command == "<" ->
  NewTapePosition = TapePosition - 1,
  processFinished(Code, Tape, NewTapePosition, InstructionPointer, LoopCounter);
processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when Command == "*" ->
  TapeValue = getSubString(Tape, TapePosition),
  NewBit = flipBit(TapeValue),
  LeftTape = string:substr(Tape, 1, TapePosition - 1),
  RightTape = string:substr(Tape, TapePosition + 1, string:length(Tape)),
  NewTape = LeftTape ++ NewBit ++ RightTape,
  processFinished(Code, NewTape, TapePosition, InstructionPointer, LoopCounter);
processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when Command == "[" ->
  processLoopOpen(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter);
processContinue(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when Command == "]" ->
  processLoopClose(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter);
processContinue(_Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) -> processFinished(Code, Tape, TapePosition, InstructionPointer, LoopCounter).

processFinished(Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
  NewInstructionPointer = InstructionPointer + 1,
  process(Code, Tape, TapePosition, NewInstructionPointer, LoopCounter).

getSubString(Tape, TapePosition) ->
  string:substr(Tape, TapePosition, 1).

flipBit(Value) when Value == "0" -> "1";
flipBit(Value) when Value == "1" -> "0".

checkOutOfBounds(Value, MaxValue, _TapePosition, _TapeLength) when Value < 1; Value > MaxValue -> out_of_bounds;
checkOutOfBounds(_Value, _MaxValue, TapePosition, TapeLength) when TapePosition < 1; TapePosition > TapeLength ->  out_of_bounds;
checkOutOfBounds(_Value, _MaxValue, _TapePosition, _TapeLength) -> ok.

processLoopOpen(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
  TapeValue = getSubString(Tape, TapePosition),
  if
    TapeValue == "0" ->
      NewInstructionPointer = InstructionPointer + 1,
      NewCommand = getSubString(Code, NewInstructionPointer),
      seekForward(NewCommand, Code, Tape, TapePosition, NewInstructionPointer, LoopCounter);
    true ->
      processFinished(Code, Tape, TapePosition, InstructionPointer, LoopCounter)
  end.

seekForward(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when LoopCounter > 0; Command /= "]" ->
  if
    Command == "[" -> NewLoopCounter = LoopCounter + 1;
    Command == "]" -> NewLoopCounter = LoopCounter - 1;
    true -> NewLoopCounter = LoopCounter
  end,
  NewInstructionPointer = InstructionPointer + 1,
  NewCommand = getSubString(Code, NewInstructionPointer),
  seekForward(NewCommand, Code, Tape, TapePosition, NewInstructionPointer, NewLoopCounter);

seekForward(_Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
  processFinished(Code, Tape, TapePosition, InstructionPointer, LoopCounter).


processLoopClose(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
  TapeValue = getSubString(Tape, TapePosition),
  if
    TapeValue /= "0" ->
      NewInstructionPointer = InstructionPointer - 1,
      NewCommand = getSubString(Code, NewInstructionPointer),
      seekBackward(NewCommand, Code, Tape, TapePosition, NewInstructionPointer, LoopCounter);
    true ->
      processFinished(Code, Tape, TapePosition, InstructionPointer, LoopCounter)
  end.

seekBackward(Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) when LoopCounter > 0; Command /= "[" ->
  if
    Command == "]" -> NewLoopCounter = LoopCounter + 1;
    Command == "[" -> NewLoopCounter = LoopCounter - 1;
    true -> NewLoopCounter = LoopCounter
  end,
  NewInstructionPointer = InstructionPointer - 1,
  NewCommand = getSubString(Code, NewInstructionPointer),
  seekBackward(NewCommand, Code, Tape, TapePosition, NewInstructionPointer, NewLoopCounter);

  seekBackward(_Command, Code, Tape, TapePosition, InstructionPointer, LoopCounter) ->
    NewInstructionPointer = InstructionPointer - 1,
    processFinished(Code, Tape, TapePosition, NewInstructionPointer, LoopCounter).