-module(gms3).
-export([start/1, start/2]).
-define(timeout, 1000).
-define(arghh, 100).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, [],1).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(),     
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves, N} ->
            Master ! joined,
            Ref = erlang:monitor(process, Leader),
            slave(Name, Master, Leader, Slaves, Ref, N,{view, Leader, Slaves, N})
        after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.

leader(Name, Master, Slaves,N) ->    
    receive
        {mcast, Msg} ->
            bcast(Name, {msg,Msg,N}, Slaves),  %% send msg to all slaves
            Master ! {deliver, Msg}, %% Deliver to application layer
            leader(Name, Master, Slaves,N+1);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name,{view, self(),NewSlaves,N},Slaves),  %% bcast NewSlaves to all slaves
            leader(Name, Master, NewSlaves,N+1);  %% update
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    
bcast(Name, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg,
                               crash(Name, Msg)
                               end, Nodes).
crash(Name, Msg) -> 
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~s CRASHED: msg ~w~n", [Name, Msg]),
            exit(no_luck);
        _ ->
            ok
        end.

slave(Name, Master, Leader, Slaves, Ref, N, Last) ->    
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg}, %% forward leader
            slave(Name, Master, Leader, Slaves, Ref,N,Last);
        {join, Peer} ->
            Leader ! {join, Peer}, %% fw join to leader
            slave(Name, Master, Leader, Slaves, Ref,N,Last);
        {msg, _, I} when I < N -> 
        	slave(Name, Master, Leader, Slaves, Ref, N, Last); %descartamos el mensaje, no ha pasado nada
        {msg, Msg, Nmesg} ->
            Master ! {deliver, Msg}, %%fw to master
            slave(Name, Master, Leader, Slaves, Ref,Nmesg+1,{msg, Msg, Nmesg});

        {view, _,_,I} when I < N -> 
        	slave(Name, Master, Leader, Slaves, Ref, N, Last); %descartamos el mensaje, no ha pasado nada
        
        {view, Leader, NewSlaves, Nmesg} ->
            slave(Name, Master, Leader, NewSlaves, Ref, Nmesg+1, {view,Leader, NewSlaves, Nmesg});  %% update new slaves
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Name, Master, Slaves,N,Last);
        {view, NewLeader, NewSlaves} ->
            erlang:demonitor(Ref, [flush]),
            NewRef = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, NewSlaves, NewRef, N, Last);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.

election(Name, Master, Slaves, N, Last) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
        	bcast(Name,Last,Rest), %Multicast last msg to all peers
            bcast(Name,{view, self(), Rest, N},Rest), 
            leader(Name, Master, Rest, N+1);  
        [NewLeader|Rest] ->
            Ref = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, Rest, Ref, N, Last)
end.
