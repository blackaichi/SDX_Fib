-module(worker).
-export([start/4]).
-define(withdrawal, 8000).

start(Name, Lock, Sleep, Work) ->
    spawn(fun() -> init(Name, Lock, Sleep, Work) end).

init(Name, Lock, Sleep, Work) ->
    Gui = gui:start(Name),
    Taken = worker(Name, Lock, [], Sleep, Work, Gui),
    Gui ! stop,
    Lock ! stop,
    terminate(Name, Taken).

worker(Name, Lock, Taken, Sleep, Work, Gui) ->
    Sleeptime = rand:uniform(Sleep),
    receive 
        stop ->
            Taken
    after Sleeptime ->
            T = acquire(Name, Lock, Gui),
            case T of
                stopped ->
                    Taken;
                withdrawn ->
                    worker(Name, Lock, [T|Taken], Sleep, Work, Gui);
                _ ->
                    Worktime = rand:uniform(Work),
                    receive 
                        stop ->
                            Gui ! leave,
                            Lock ! release,
                            Taken
                    after Worktime ->
                            io:format("~s: lock released~n", [Name]),
                            Gui ! leave,
                            Lock ! release,
                            worker(Name, Lock, [T|Taken], Sleep, Work, Gui)
                    end
            end
    end.

acquire(Name, Lock, Gui) ->
  T1 = erlang:monotonic_time(),
  Gui ! waiting,
  Ref = make_ref(),
  Lock ! {take, self(), Ref},
  receive
      {taken, Ref} ->
          T2 = erlang:monotonic_time(),
          T = erlang:convert_time_unit(T2-T1, native, millisecond),
          io:format("~s: lock taken in ~w ms~n", [Name, T]),
          Gui ! taken,
          {taken, T};
      stop ->
          Gui ! leave,
          Lock ! release,
          stopped
  after ?withdrawal ->
          io:format("~s: giving up~n", [Name]),
          Gui ! leave,
          Lock ! release,
          withdrawn
  end.

terminate(Name, Taken) ->
    {Locks, Time, Dead} = 
       lists:foldl(
          fun(Entry,{L,T,D}) -> 
            case Entry of 
               {taken,I} -> 
                   {L+1,T+I,D}; 
                _ -> 
                   {L,T,D+1} 
            end
          end, 
          {0,0,0}, Taken),
    if 
       Locks > 0 ->
           Average = Time / Locks;
       true ->
           Average = 0
    end,
    io:format("~s: ~w locks taken, ~w ms (avg) for taking, ~w withdrawals~n", 
              [Name, Locks, Average, Dead]).
