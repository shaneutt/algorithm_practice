-module('dijkstra').
-include_lib("eunit/include/eunit.hrl").

-export([dijkstrafy/3]).

%%====================================================================
%% API functions
%%====================================================================

% just hide away recursion so we have a nice interface
dijkstrafy(Graph, Start, End) when is_map(Graph) ->
	shortest_path(Graph, [{0, [Start]}], End, #{}).

%%====================================================================
%% Internal functions
%%====================================================================

% if we're not going anywhere, it's time to start going back up the chain.
shortest_path(_Graph, [], _End, _Visited) -> {0, []};
% this is the last case we would hit, and finally returns the distance
% and the path from Start to End. Because of Erlang's pattern matching
% being so damn good, this will check that the first set in "routes"
% is the end, we'll know we're done and can send back the results.
% this is positionally important, because the final recursive case
% will always match something, we could have a condition where it would
% loop infinitely in the third function head for "shortest_path".
shortest_path(_Graph, [{Cost, [End | _] = Path} | _ ], End, _Visited) ->
	{Cost, lists:reverse(Path)};
% this is the recursive case, or "main loop".
shortest_path(Graph, [{Cost, [Node | _ ] = Path} | Routes], End, Visited) ->
	% build a list of new "unvisited" routes, where the stucture is
	% a tuple of cost, then a list of paths taken to get to that cost
	% from the "Start"
	NewRoutes = [{Cost + NewCost, [NewNode | Path]}
		|| {NewCost, NewNode} <- maps:get(Node, Graph),
			not maps:get(NewNode, Visited, false)],
	shortest_path(
		Graph,
		% add the routes we ripped off earlier onto the new routes
		% that we want to visit. sort the list of routes to get the
		% shortest routes (lowest cost) at the beginning.
		% Erlangs sort is already good enough, and it will sort the
		% tuples by the number at the beginning of each (the cost).
		lists:sort(NewRoutes ++ Routes),
		End,
		Visited#{Node => true}
	).

%%====================================================================
%% Tests
%%====================================================================

basic_test() ->
	Graph = #{
		abode     => [{4,casa},{8,apartment},{9,crib},{18,pad}],
		crib      => [{9,abode},{9,farm},{12,flat},{19,pad}],
		dwelling  => [{13,casa},{17,apartment},{18,haunt},{20,digs}],
		casa      => [{4,abode},{13,dwelling},{14,pad},{19,digs}],
		condo     => [{6,den},{7,apartment},{10,farm},{11,flat}],
		farm      => [{3,flat},{9,crib},{10,condo}],
		den       => [{6,condo},{9,digs},{10,haunt}],
		flat      => [{3,farm},{11,condo},{12,crib},{12,digs},{15,pad}],
		pad       => [{14,haunt},{14,casa},{15,flat},{18,abode},{19,crib}],
		apartment => [{7,condo},{8,abode},{17,dwelling}],
		haunt     => [{10,den},{14,pad},{19,dwelling}],
		digs      => [{9,den},{12,flat},{19,casa},{20,dwelling}]
	},
	{31, [abode,apartment,condo,den,haunt]} = dijkstrafy(Graph, abode, haunt),
	{17, [farm,condo,apartment]}            = dijkstrafy(Graph, farm, apartment),
	{33, [crib,pad,haunt]}                  = dijkstrafy(Graph, crib, haunt),
	{25, [den,condo,apartment,abode,casa]}  = dijkstrafy(Graph, den, casa),
	{25, [casa,abode,apartment,condo,den]}  = dijkstrafy(Graph, casa, den).

