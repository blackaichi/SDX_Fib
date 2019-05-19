-module(resolver).
-export([start/1, stop/0]).
-define(timeout, 1000).

start(Root) ->
    register(resolver, spawn(fun()-> init(Root) end)).

stop() ->
    resolver ! stop,
    unregister(resolver).

init(Root) ->
    Cache = [],
    NewCache = cache:add([], inf, {domain, Root}, Cache),
    resolver(NewCache).

resolver(Cache) ->
    receive
        {request, From, Req}->
            io:format("Resolver: request from ~w to solve ~w~n", [From, Req]),
            {Reply, NewCache} = resolve(Req, Cache, []),
            From ! {reply, Reply},
            resolver(NewCache);
        status ->
            io:format("Resolver: cache content: ~w~n", [Cache]),
            resolver(Cache);
        stop ->
            io:format("Resolver: closing down~n", []),
            ok;
        Error ->
            io:format("Resolver: reception of strange message ~w~n", [Error]),
            resolver(Cache)
    end.

resolve(Name, Cache, Req)->
    io:format("Resolve ~w: ", [Name]),
    case cache:lookup(Name, Cache) of
        unknown ->
            io:format("unknown ~n", []),
            [Head|Domain] = Name,
            resolve(Domain, Cache, lists:append(Req, [Head]));
        invalid ->
            io:format("invalid ~n", []),
            [Head|Domain] = Name,
            NewCache = cache:remove(Name, Cache),
            resolve(Domain, NewCache, lists:append(Req, [Head]));
        {domain, Srv} ->
            io:format("found domain ~w~n", [Srv]),
            case Req of
                [] ->
                    {{domain, Srv}, Cache};
                _ ->
                    Srv ! {request, self(), Req},
                    io:format("Resolver: sent request to solve ~w to ~w: ", [Req, Srv]),
                    receive
                        {reply, unknown} ->
                            io:format("unknown ~n", []),
                            {unknown, Cache};
                        {reply, Replies} ->
                            {Reply, Pid, _} = lists:last(Replies),
                            NewReplies = lists:map(fun({N, E, T}) ->
                                FullName = lists:append(N, Name),
                                {FullName, E, T}
                                end, Replies),
                            NewCache = updatecache(NewReplies, Cache),
                            io:format("reply ~w ~w - ", [Reply, Pid]),
                            io:format("ALL ~w Cached ~w~n", [Replies, NewReplies]),
                            {Pid, NewCache}
                    after ?timeout ->
                        io:format("timeout~n", []),
                        {unknown, Cache}
                    end
            end;
        {host, Pid} ->
            io:format("found host~n", []),
            {{host, Pid}, Cache}
    end.

updatecache([], Cache) ->
    Cache;
updatecache([{Name,Entry,TTL}|Replies], Cache) ->
    Now = erlang:monotonic_time(),
    Expire = erlang:convert_time_unit(Now, native, second) + TTL,
    NewCache = cache:add(Name, Expire, Entry, Cache),
    updatecache(Replies, NewCache).
