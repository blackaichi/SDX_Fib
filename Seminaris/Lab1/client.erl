-module(client).
%% Exported Functions
-export([start/2]).

start(ServerPid, MyName) ->
    ClientPid = spawn(fun() -> init_client(ServerPid, MyName) end),
    process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
    ServerPid ! {client_join_req, MyName, self()},  %% TODO: COMPLETE
    process_requests().

%% This is the background task logic
process_requests() ->
    receive
        {join, Name} ->
            io:format("[JOIN] ~s joined the chat~n", [Name]),
            process_requests();
        {leave, Name} ->
            io:format("[LEAVE] ~s left the chat~n", [Name]),
            process_requests();
        {message, Name, Text} ->
            io:format("[~s] ~s", [Name, Text]),
            process_requests();
        exit -> 
            ok
    end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
    %% Read from standard input and send to server
    Text = io:get_line("-> "),
    if 
        Text  == "exit\n" ->
            ServerPid ! {client_leave_req, MyName, ClientPid},  %% TODO: COMPLETE
            ok;
        true ->
            ServerPid ! {send, MyName, Text},  %% TODO: COMPLETE
            process_commands(ServerPid, MyName, ClientPid)
    end.
