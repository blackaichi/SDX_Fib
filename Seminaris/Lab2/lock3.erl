-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
	Clock = 0,
    receive
        {peers, Nodes} ->
            open(Nodes,MyId,Clock);
        stop ->
            ok
    end.

open(Nodes,MyId,Clock) ->
    receive
        {take, Master, Ref} ->
			SegClock = Clock+1,
            Refs = requests(Nodes,MyId,SegClock),
            wait(Nodes, Master, Refs, [], Ref, MyId,SegClock,SegClock);
            
        {request, From,  Ref,_,Clockreq} ->
            From ! {ok, Ref},
            open(Nodes,MyId,max(Clock,Clockreq));
        stop ->
            ok
    end.

requests(Nodes,MyId,Clock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId,Clock}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId,Clock,SegClock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting,MyId,SegClock);
    
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId,Clock,SegClock) ->
    receive
        {request, From, Ref, Id, Clockreq} ->
            if
                Clockreq < Clock -> 
					From ! {ok, Ref},
                    From ! {request, self(), Ref, MyId,max(Clockreq,SegClock)},
                    wait(Nodes, Master, [Ref|Refs], Waiting, TakeRef,MyId,Clock,max(Clockreq,SegClock));
                
                Clockreq == Clock ->
					if 
						Id < MyId -> 
							From ! {ok, Ref},
							From ! {request, self(), Ref, MyId,max(Clockreq,SegClock)},
							wait(Nodes, Master, [Ref|Refs], Waiting, TakeRef,MyId,Clock,max(Clockreq,SegClock));
						true ->
							wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef,MyId,Clock,max(Clockreq,SegClock))
					end;
							
                
				true -> 
					wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef,MyId,Clock,max(Clockreq,SegClock))
            end;
        
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef,MyId,Clock,SegClock);
        release ->
            ok(Waiting),            
            open(Nodes, MyId,SegClock)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting,MyId,Clock) ->
    receive
        {request, From, Ref, ClockReq} ->
            held(Nodes, [{From, Ref}|Waiting],MyId,max(ClockReq,Clock));
        release ->
            ok(Waiting),
            open(Nodes,MyId,Clock)
    end.
