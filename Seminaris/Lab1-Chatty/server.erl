-module(server).
%% Exported Functions
-export([start/0]).

start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [From|Clients],  %% TODO: COMPLETE
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  %% TODO: COMPLETE
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From, Clients),  %% TODO: COMPLETE
            broadcast(Clients, {leave, Name}),  %% TODO: COMPLETE
            From ! exit,
            process_requests(NewClients);  %% TODO: COMPLETE
        {send, Name, Text} ->
			io:format("Mensaje! ~s ~s",[Name,Text]),
            broadcast(Clients, {message, Name, Text}),  %% TODO: COMPLETE
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
