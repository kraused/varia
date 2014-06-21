
-module(hello).

-behavior(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	hello_sup:start_link(StartArgs).

stop(_State) ->
	ok.

