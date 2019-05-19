-module(server).
-export([start/0, start/2, stop/0]).

start() ->
    register(server, spawn(fun()-> init() end)).

start(Domain, Parent) ->
    register(server, spawn(fun()-> init(Domain, Parent) end)).

stop() ->
    server ! stop,
    unregister(server).

init() ->
    io:format("Server: create root domain~n"),
    server([], 0).

init(Domain, Parent) ->
    io:format("Server: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {domain, self()}},
    server([], 0).

server(Entries, TTL) ->
    receive
        {request, From, Req}->
            io:format("Server: received request to solve [~w]~n", [Req]),
            Reply = entry:lookup(Req, Entries),
            From ! {reply, Reply, TTL},
            server(Entries, TTL);
        {register, Name, Entry} ->
            NewEntries = entry:add(Name, Entry, Entries),
            server(NewEntries, TTL);
        {deregister, Name} ->
            NewEntries = entry:remove(Name, Entries),
            server(NewEntries, TTL);
        {ttl, Sec} ->
            server(Entries, Sec);
        status ->
            io:format("Server: List of DNS entries: ~w~n", [Entries]),
            server(Entries, TTL);
        stop ->
            io:format("Server: closing down~n", []),
            ok;
        Error ->
            io:format("Server: reception of strange message ~w~n", [Error]),
            server(Entries, TTL)
    end.
