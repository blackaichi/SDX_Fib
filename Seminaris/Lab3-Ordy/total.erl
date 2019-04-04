-module(total).
-export([start/3]).

start(Id, Master, Jitter) ->
    spawn(fun() -> init(Id, Master, Jitter) end).

init(Id, Master, Jitter) ->
    receive
        {peers, Nodes} ->
            server(Master, seq:new(Id), seq:new(Id), Nodes, [], [], Jitter)
    end.

server(Master, MaxPrp, MaxAgr, Nodes, Cast, Queue, Jitter) ->
receive
    {send, Msg} ->
        Ref = make_ref(),
        request(Ref , Msg , Nodes , Jitter),
        NewCast = cast(Ref , Nodes , Cast),
        server(Master, MaxPrp, MaxAgr, Nodes, NewCast, Queue, Jitter);
    {request, From, Ref, Msg} ->
        NewMaxPrp = seq:increment(seq:maxfirst(MaxPrp, MaxAgr)) ,
        From ! {proposal, Ref , NewMaxPrp},
        NewQueue = insert(Ref, Msg, NewMaxPrp, Queue),
        server(Master, NewMaxPrp, MaxAgr, Nodes, Cast, NewQueue, Jitter);
    {proposal, Ref, Proposal} ->
        case proposal(Ref, Proposal, Cast) of
            {agreed, MaxSeq, NewCast} ->
                agree(Ref, MaxSeq, Nodes),
                server(Master, MaxPrp, MaxSeq, Nodes, NewCast, Queue, Jitter);
            NewCast ->
                server(Master, MaxPrp, MaxAgr, Nodes, NewCast, Queue, Jitter) 
        end;
    {agreed, Ref, Seq} ->
        NewQueue = update(Ref , Seq, Queue),
        {AgrMsg, NewerQueue} = agreed(NewQueue),
        deliver(Master , AgrMsg),
        NewMaxAgr = max(maxAgr, Seq) ,
        server(Master, MaxPrp, NewMaxAgr, Nodes, Cast, NewerQueue, Jitter);
    stop ->
        ok
end.

%% Sending a request message to all nodes
request(Ref, Msg, Nodes, 0) ->
    Self = self(),
    lists:foreach(fun(Node) -> 
                      Node ! {request, Self, Ref, Msg} %% TODO: ADD SOME CODE
                  end, 
                  Nodes);
request(Ref, Msg, Nodes, Jitter) ->
    Self = self(),
    lists:foreach(fun(Node) ->
                      T = rand:uniform(Jitter),
                      timer:send_after(T, Node, {request, Self, Ref, Msg} ) %% TODO: COMPLETE
                  end,
                  Nodes).
        
%% Sending an agreed message to all nodes
agree(Ref, Seq, Nodes)->
    lists:foreach(fun(Pid)-> 
                      Pid ! {agreed, Ref, Seq} %% TODO: ADD SOME CODE
                  end, 
                  Nodes).

%% Delivering messages to the master
deliver(Master, Messages) ->
    lists:foreach(fun(Msg)-> 
                      Master ! {deliver, Msg} 
                  end, 
                  Messages).
                  
%% Adding a new entry to the set of casted messages
cast(Ref, Nodes, Cast) ->
    L = length(Nodes),
    [{Ref, L, seq:null()}|Cast].

%% Update the set of pending proposals
proposal(Ref, Proposal, [{Ref, 1, Max}|Rest])->
    {agreed, seq:max(Proposal, Max), Rest};
proposal(Ref, Proposal, [{Ref, N, Max}|Rest])->
    [{Ref, N-1, seq:max(Proposal, Max)}|Rest];
proposal(Ref, Proposal, [Entry|Rest])->
    case proposal(Ref, Proposal, Rest) of
        {agreed, AgrNum, NewRest} ->
            {agreed, AgrNum, [Entry|NewRest]};
        Updated ->
            [Entry|Updated]
    end.
    
%% Remove all messages in the front of the queue that have been agreed
agreed([{_Ref, Msg, agrd, _Agr}|Queue]) ->
    {AgrMsg, NewQueue} = agreed(Queue),
    {[Msg|AgrMsg], NewQueue};
agreed(Queue) ->
    {[], Queue}.
    
%% Update the queue with an agreed sequence number
update(Ref, AgrNum, [{Ref, Msg, propsd, _}|Rest])->
    queue(Ref, Msg, agrd, AgrNum, Rest);
update(Ref, AgrNum, [Entry|Rest])->
    [Entry|update(Ref, AgrNum, Rest)].
    
%% Insert a new message into the queue
insert(Ref, Msg, Proposal, Queue) ->
    queue(Ref, Msg, propsd, Proposal, Queue).
    
%% Queue a new entry using Proposal as key
queue(Ref, Msg, State, Proposal, []) ->
    [{Ref, Msg, State, Proposal}];
queue(Ref, Msg, State, Proposal, Queue) ->
    [Entry|Rest] = Queue,
    {_, _, _, Next} = Entry,
    case seq:lessthan(Proposal, Next) of
        true ->
            [{Ref, Msg, State, Proposal}|Queue];
        false ->
            [Entry|queue(Ref, Msg, State, Proposal, Rest)]
    end.
