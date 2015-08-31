package ShanesDijkstra

class Graph {
	/* first we take in our arguments with a nice clean interface
	 * that takes a map as graph, a string for start node, and a
	 * a string for end node.
	 */
	def Dijkstrafy(
		graph: Map[String, List[(Int, String)]],
		start: String,
		end:   String
	/* our function returns an integer of the cost to reach the end
	 * from start, and a list of strings of the nodes traveled to
	 * the shortest path.
	 */
	) : (Int, List[String]) = {
		ShortestPath(
			graph,
			List((0, List(start))),
			end,
			Map()
		)
	}
	/* this is the recursive case, or "main loop" */
	private def ShortestPath(
		graph:   Map[String, List[(Int, String)]],
		/* now we've added a list of strings because every time
		 * we loop we're potentially adding nodes that lead up
		 * to a node from "start"
		 */
		routes:  List[(Int, List[String])],
		end:     String,
		/* "visited" will be a map of nodes as strings, and will
		 * just be "true" when a node is visited
		 */
		visited: Map[String, Boolean]
		/* Scala doesn't seem to have the pattern matching Erlang has,
		 * so case statements are the path forkers for the recursion
		 */
	) : (Int, List[String]) = routes match {
		/* grab the top route off, and put the remainder in "remaining
		 * routes". then do a match on the path
		 */
		case (cost, path) :: remaining_routes => path match {
			/* get the first node in the path, and place the rest in
			 * "remaining_nodes".
			 */
			case node :: remaining_nodes => {
				/* this is the last case, where we will finally
				 * return our output when we've reach the end node
				 */
				if (node == end) (cost, path.reverse)
				/* this is the normal case. we'll get all the
				 * routes for the node we're working on out of
				 * our graph map, then we'll do a list comprehension
				 * on those routes to "yield" all the new routes we
				 * need to visit.
				 */
				else {
					var routes    = graph(node)
					/* yield a new route as a tuple of cost, and
					 * path used to get there for each route we
					 * have not visited yet.
					 */
					var newroutes = for (route <- routes if !visited.contains(route._2))
						yield (cost + route._1, route._2 :: path)
					/* put all our routes back together including our
					 * new routes, but then sort them so that the
					 * lowest cost paths come up sooner in the list
					 */
					val allroutes     = (newroutes ++ remaining_routes)
					val sorted_routes = allroutes.sortWith(_._1 < _._1)
					/* do it all again! */
					ShortestPath(graph, sorted_routes, end, visited + (node -> true))
				}
			}
		}
	}
}
