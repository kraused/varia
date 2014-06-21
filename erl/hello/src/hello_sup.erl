
-module(hello_sup).

-behavior(supervisor).
-export([start_link/1, init/1]).


start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
	{ok, {{one_for_one, 3, 100},	% FIXME Optimize parameters
	    [{tag1,
	        {hello_serv, start_link, Args},
	        permanent,
	        1000,			% FIXME Optimize parameters
	        worker,
	        [hello_serv]}]}}.

