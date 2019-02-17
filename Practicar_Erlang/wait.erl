-module(wait).
-export([hello/0]).

hello() ->
	receive
		X -> io:format("haaaaa msg: ~s~n", [X])
	end.