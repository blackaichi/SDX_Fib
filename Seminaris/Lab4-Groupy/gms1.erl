-module(gms1).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(), 
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            slave(Name, Master, Leader, Slaves)
    end.

leader(Name, Master, Slaves) ->    
    receive
        {mcast, Msg} ->
            bcast(Name, {msg,Msg}, Slaves),  %% send msg to all slaves
            Master ! {deliver, Msg}, %% Deliver to application layer
            leader(Name, Master, Slaves);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name,{view, self(),NewSlaves},Slaves),  %% bcast NewSlaves to all slaves
            leader(Name, Master, NewSlaves);  %% update
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    
bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

slave(Name, Master, Leader, Slaves) ->    
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg}, %% forward leader
            slave(Name, Master, Leader, Slaves);
        {join, Peer} ->
            Leader ! {join, Peer}, %% fw join to leader
            slave(Name, Master, Leader, Slaves);
        {msg, Msg} ->
            Master ! {deliver, Msg}, %%fw to master
            slave(Name, Master, Leader, Slaves);
        {view, Leader, NewSlaves} ->
            slave(Name, Master, Leader, NewSlaves);  %% update new slaves
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.
