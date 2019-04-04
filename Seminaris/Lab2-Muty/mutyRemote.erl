-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
%    register(l1, apply(Lock, start, [1])),
%    register(l2, apply(Lock, start, [2])),
%    register(l3, apply(Lock, start, [3])),
%    register(l4, apply(Lock, start, [4])),
%    l1 ! {peers, [l2, l3, l4]},
%    l2 ! {peers, [l1, l3, l4]},
%    l3 ! {peers, [l1, l2, l4]},
%    l4 ! {peers, [l1, l2, l3]},
%    register(w1, worker:start("PeppaPig", l1, Sleep, Work)),
%    register(w2, worker:start("Ringo", l2, Sleep, Work)),    
%    register(w3, worker:start("Paul", l3, Sleep, Work)),
%    register(w4, worker:start("George", l4, Sleep, Work)),

%Node1 -> node1@IPLOCALHOST
%{l1, Node1} ! {peers, [l2, l3, l4]} enviar msg a la instancia l1 del node1
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    Node4 = 'node4@127.0.0.1',
    
    spawn(Node1, fun() ->
        register(l1, apply(Lock, start, [1])),
        register(w1, worker:start("PeppaPig", l1, Sleep, Work)) end),
        
    spawn(Node2, fun() ->
        register(l2, apply(Lock, start, [2])),
        register(w2, worker:start("Ringo", l2, Sleep, Work)) end),
        
    spawn(Node3, fun() ->
        register(l3, apply(Lock, start, [3])),
        register(w3, worker:start("Paul", l3, Sleep, Work)) end),
         
    spawn(Node4, fun() ->
        register(l4, apply(Lock, start, [4])),
        register(w4, worker:start("George", l4, Sleep, Work)) end),
    timer:sleep(1000),
    {l1, Node1} ! {peers, [{l2,Node2}, {l3,Node3}, {l4,Node4}]},
    {l2, Node2} ! {peers, [{l1,Node1}, {l3,Node3}, {l4,Node4}]},
    {l3, Node3} ! {peers, [{l2,Node2}, {l1,Node1}, {l4,Node4}]},
    {l4, Node4} ! {peers, [{l2,Node2}, {l3,Node3}, {l1,Node1}]},
    
    ok.

stop() ->
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    Node4 = 'node4@127.0.0.1',
    {w1, Node1} ! stop,
    {w2, Node2} ! stop,
    {w3, Node3} ! stop,
    {w4, Node4} ! stop.
