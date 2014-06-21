
-module(hello_serv).

-export([start_link/0]).

start_link() ->
	spawn_link(serv()).

serv() ->
	{ok, Listen} = gen_tcp:listen(3000, [binary, {packet, 0},
	                                     {reuseaddr, false},
	                                     {active, true}]),
	loop(Listen).

loop(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	handle(Socket),
	loop(Listen).

handle(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("received binary = ~p~n", [Bin]),
			gen_tcp:send(Socket, Bin),
			handle(Socket);
		{tcp_closed, Socket} ->
			io:format("socket closed~n")	
	end.

