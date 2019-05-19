-module(cache).
-export([lookup/2, add/4, remove/2]).

lookup(Name, Cache) -> 
	Now = erlang:monotonic_time(),
	Expiration = erlang:convert_time_unit(Now, native, second),
    case lists:keyfind(Name, 1, Cache) of
    	false -> unknown;
    	{_, Expire, Reply} -> 
    		if 
    			Expire < Expiration ->
    				invalid;
    				
    			true -> Reply
    		end
    end.

add(Domains, Expire, Reply, Cache) ->
	lists:keystore(Domains, 1, Cache, {Domains, Expire, Reply}).

remove(Name, Cache) ->
	lists:keydelete(Name, 1, Cache).
