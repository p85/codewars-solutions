-module(solution).
-export([whiner/1]).

whiner(Parent) ->
  receive
    {awake} ->
      Pid = spawn(fun() -> sendMessage(Parent) end),
      register(subproc, Pid);
    {give_up} ->
      ThePid = whereis(subproc),
      exit(ThePid, ok),
      exit(self(), ok);
        M -> M
  end,
  whiner(Parent).

sendMessage(Parent) ->
  Parent ! {whine, "Is anybody out there?"},
  timer:sleep(1000),
  sendMessage(Parent).