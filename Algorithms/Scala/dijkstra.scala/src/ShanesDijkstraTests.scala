import ShanesDijkstra._

object ShanesDijkstraTests {
	def main (x: Array[String]) : Unit = {
		val graph = Map(
			"abode"     -> List((4, "casa"), (8, "apartment"), (9, "crib"), (18, "pad")),
			"crib"      -> List((9, "abode"), (9, "farm"), (12, "flat"), (19, "pad")),
			"dwelling"  -> List((13, "casa"), (17, "apartment"), (18, "haunt"), (20, "digs")),
			"casa"      -> List((4, "abode"), (13, "dwelling"), (14, "pad"), (19, "digs")),
			"condo"     -> List((6, "den"), (7, "apartment"), (10, "farm"), (11, "flat")),
			"farm"      -> List((3, "flat"), (9, "crib"), (10, "condo")),
			"den"       -> List((6, "condo"), (9, "digs"), (10, "haunt")),
			"flat"      -> List((3, "farm"), (11, "condo"), (12, "crib"), (12, "digs"), (15, "pad")),
			"pad"       -> List((14, "haunt"), (14, "casa"), (15, "flat"), (18, "abode"), (19, "crib")),
			"apartment" -> List((7, "condo"), (8, "abode"), (17, "dwelling")),
			"haunt"     -> List((10, "den"), (14, "pad"), (19, "dwelling")),
			"digs"      -> List((9, "den"), (12, "flat"), (19, "casa"), (20, "dwelling"))
		)
		val dijkstrafier = new ShanesDijkstra.Graph
		/*********************
		 ** FIRST TEST CASE **
		 *********************/
		println("testing shortest path for: abode to haunt")
		val result = dijkstrafier.Dijkstrafy(graph, "abode", "haunt")
		run_test(result, 31, List("abode", "apartment", "condo", "den", "haunt"))
		/**********************
		 ** SECOND TEST CASE **
		 **********************/
		println("testing shortest path for: farm to apartment")
		val result2 = dijkstrafier.Dijkstrafy(graph, "farm", "apartment")
		run_test(result2, 17, List("farm", "condo", "apartment"))
		/*********************
		 ** THIRD TEST CASE **
		 *********************/
		println("testing shortest path for: crib to haunt")
		val result3 = dijkstrafier.Dijkstrafy(graph, "crib", "haunt")
		run_test(result3, 33, List("crib", "pad", "haunt"))
		/**********************
		 ** FOURTH TEST CASE **
		 **********************/
		println("testing shortest path for: den to casa")
		val result4 = dijkstrafier.Dijkstrafy(graph, "den", "casa")
		run_test(result4, 25, List("den", "condo", "apartment", "abode", "casa"))
		/*********************
		 ** FIFTH TEST CASE **
		 *********************/
		println("testing shortest path for: casa to den")
		val result5 = dijkstrafier.Dijkstrafy(graph, "casa", "den")
		run_test(result5, 25, List("casa", "abode", "apartment", "condo", "den"))
	}
	private def run_test (
		result:        (Int, List[String]),
		expected_cost: Int,
		expected_path: List[String]
	) : Boolean = {
		println(result)
		val cost = result._1
		val path = result._2
		if (cost != expected_cost) {
			println("failure!\n")
			return false
		}
		if (!compare_lists(path, expected_path)) {
			println("failure!\n")
			return false
		}
		println("success!\n")
		return true
	}
	private def compare_lists (
		list1: List[String],
		list2: List[String]
	) : Boolean = {
		if (list1.size != list2.size) {
			return false
		}
		list1.foreach {
			case (i) =>
				if (!list1.contains(i)) {
					return false
				}
		}
		return true
	}
}

