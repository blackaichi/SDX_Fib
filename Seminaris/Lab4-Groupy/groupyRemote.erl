-module(groupyRemote).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    Node4 = 'node4@127.0.0.1',
    Node5 = 'node5@127.0.0.1',
    
    
    spawn(Node1, fun() ->
         register(a, worker:start("P1", Module, Sleep)) end),
         
    spawn(Node2, fun() ->
        register(b, worker:start("P2", Module, {a,Node1}, Sleep)) end),
        
    spawn(Node3, fun() ->
        register(c, worker:start("P3", Module, {a,Node1}, Sleep)) end),
    
    spawn(Node4, fun() ->
        register(d, worker:start("P4", Module, {a,Node1}, Sleep)) end),
    
    spawn(Node5, fun() ->
        register(e, worker:start("P5", Module, {a,Node1}, Sleep)) end).
    

    

stop() ->
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    Node4 = 'node4@127.0.0.1',
    Node5 = 'node5@127.0.0.1',
    
    stop({a,Node1}),
    stop({b,Node2}),
    stop({c,Node3}),
    stop({d,Node4}),
    stop({e,Node5}).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

