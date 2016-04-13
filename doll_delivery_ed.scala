//Doll-delivery
//Author: Edward Ma - amde@umich.edu
//Djikstra's Algorithm in Scala 2.11.8

//according to github the Map will be read in as a list of nodes, each of which is
//a scala map. Input also includes starting + target location

//I tried to format it in a comfortable way (I am used to OOP) so it is less 'functional' than it should be
//but it is still in scala!!


//import scala.io.Source;
import scala.collection.mutable;
//import scala.util.control.Breaks._

//Main program object
object doll_delivery_ed {
  
  /* FUNCTION: Creates the set of unvisited nodes from the map of edges by adding to an empty set that is passed into the function
   * REQUIRES: A valid scala List of "edges" represented as Maps containing one node to another + distance
   * 					 A valid, empty set of Strings
   * RETURNS:  Nothing.
   */
  def create_unvisited_set(edges:List[Map[String, Any]], unvisited_set:scala.collection.mutable.Set[String]): Unit = {
    	for (i <- edges) {
		  	if (!unvisited_set.contains(i("startLocation").toString)) {
		  		unvisited_set += i("startLocation").toString;
		  	}
		  	if (!unvisited_set.contains(i("endLocation").toString)) {
		    	unvisited_set += i("endLocation").toString;
		  	}
		}
  }

  /* FUNCTION: Compiles recorded path into a string from the Map.
   * REQUIRES: A valid mutable scala Map structure with a string key and "Any" type value (null + String) 
   * RETURNS: String containing the path formatted in the way desired in the postnati Github spec
   */
	def compile_path(path:mutable.Map[String,Any], finish:String): String = {
		var backtrack = mutable.ArrayBuffer[String] (finish);
		var backtrack_step = finish; //string
		var soln:String = "";
		while (path(backtrack_step) != null) { //while the path's END value is not the start (as we backtrack)
			path(backtrack_step).toString +=: backtrack; //prepend to the backtrack string data structure
			backtrack_step = path(backtrack_step).toString;
		}
		//backtrack.toArray;
		//println(backtrack);
		for (path_string <- backtrack) {
			soln = soln + path_string + " => ";
		}
		soln = soln.dropRight(3);

		return soln;
	} //end compile_path()


	//dijkstra algorithm function
	/* FUNCTION: Calculates then prints the min distance path of the map from one node to another. Basic error checking for user errors built in.
	 * 						Also walks user through the algorithm thru print statements.
	 * REQUIRES: A valid edges list formatted as a List of "edges" represented as Maps connecting one node to another + distance (as seen in postnati Github spec)
	 * 					 THE GRAPH MUST BE CONNECTED
	 * RETURNS: nothing
	 */
	def dijkstra_alg(start:String, finish:String, edges:List[Map[String, Any]]): Unit = {

		//ERROR CHECKING 1: if empty map then exit(1)
	  	if (edges.isEmpty) {
	    	println("empty map input error, exiting");
	    	System.exit(1);
	  	}
	  
		var unvisited_set = mutable.Set[String]("1");
		unvisited_set.clear(); //wasn't sure how to declare an empty set
		create_unvisited_set(edges, unvisited_set); //creates our unvisited set

		
		
		
		
		// ERROR CHECKING 2: if edges map doesn't contain start and finish nodes then something is wrong with user's input
		if (!unvisited_set.contains(start)) {
			println("Start node does not exist");
			println("Proper function call: dijkstra_alg(<valid node(String)>, <valid node(String)>, List(Map(node(str), node(str), distance(int)), Map...)");
		  
		  	System.exit(1);
		} else if (!unvisited_set.contains(finish)) {
			println("Finish node does not exist");
			println("Proper function call: dijkstra_alg(<valid node(String)>, <valid node(String)>, List(Map(node(str), node(str), distance(int)), Map...)");
		  
		  	System.exit(1);

		}
		
		
		
		
		println("Unvisited set initial:"); //show user initial unvisited sets
		println(unvisited_set);

		//would use int to save memory, but scala has Double.PositiveInfinity method
		val init_distance = collection.mutable.Map[String, Double] (start -> 0);
		val path = collection.mutable.Map[String, Any] (start -> null);


		//set of the name of unvisited nodes w/ least distance
		for (n <- edges) {
			//initialize start location distances + path data structs
			if (n("startLocation") != start) {
				if (!init_distance.contains(n("startLocation").toString)) { //no repeats
					init_distance += (n("startLocation").toString -> Double.PositiveInfinity);
					path += (n("startLocation").toString -> null);
				}
			}
			//initialize end location distances + path data structs. 
			if (n("endLocation") != start) {
				if (!init_distance.contains(n("endLocation").toString)) {//no repeats
					init_distance += (n("endLocation").toString -> Double.PositiveInfinity);
					path += (n("endLocation").toString -> null);
				}
			}
		};
		
		
		/* DEBUGGING */
	/*	println("After Initialization:");
		println( "Keys in mindistance : " + init_distance.keys )
      	println( "Values in mindistance : " + init_distance.values )
     	println( "Check if mindistance is empty : " + init_distance.isEmpty )
      
      
      	println( "Keys in path : " + path.keys )
      	println( "Values in path : " + path.values )
      	println( "Check if path is empty : " + path.isEmpty )
      
      	println();
	*/



		var debugcount = 0;

		while (!unvisited_set.isEmpty) {
			var current: String = "";

			
			println("visiting nodes loop: " + (debugcount + 1));
		  	//println( "Keys in mindistance : " + init_distance.keys )
      		//println( "Values in mindistance : " + init_distance.values )
      		//println( "Check if mindistance is empty : " + init_distance.isEmpty )
      		//println( "Keys in path : " + path.keys )
      		//println( "Values in path : " + path.values )
      		//println( "Check if path is empty : " + path.isEmpty )

			/* 
			 * if the node is unvisited, set 'current node' with the 
			 * node w/ smallest distance (starting with 0, obviously)
			 * if it is visited get rid of the node in Map min_distance 
			 */
      		var temp = init_distance.clone();//FINALLY FIGURED OUT WITHOUT CLONE IT ACTS AS A POINTER
			while (current.length() == 0){ // while current node does not exist
				if (unvisited_set.contains(temp.minBy(_._2)._1.toString)) {
					current = temp.minBy(_._2)._1;
				} else {
					temp -= temp.minBy(_._2)._1; 
							  /*println( "Keys in mindistance : " + init_distance.keys )
      println( "Values in mindistance : " + init_distance.values )
      println( "Check if mindistance is empty : " + init_distance.isEmpty )*/

				}
			}
		  
			print("         after setting current node: ");
			println(current);
		  	//println( "Keys in mindistance : " + init_distance.keys )
      		//println( "Values in mindistance : " + init_distance.values )
      		//println( "Check if mindistance is empty : " + init_distance.isEmpty )

			
			//to get here we have a node that exists
			//println(unvisited_set);
			unvisited_set = unvisited_set - current; //"visit" node
			print("need to visit nodes: ");
			println(unvisited_set);

		
		  //check neighbors
		  	for (node_name <- edges) {
		    	if (current == node_name("startLocation")) {
		      		var alt_min_dist = init_distance(current).toInt + node_name("distance").toString.toInt;
		      		if (alt_min_dist < init_distance(node_name("endLocation").toString)) {
		        		init_distance(node_name("endLocation").toString) = alt_min_dist;
		        		path(node_name("endLocation").toString) = current;
		      		}
		      
		    	} else if (current == node_name("endLocation")) {
		      		var alt_min_dist = init_distance(current).toInt + node_name("distance").toString.toInt;
		      		if (alt_min_dist < init_distance(node_name("startLocation").toString)) {
		        		init_distance(node_name("startLocation").toString) = alt_min_dist;
		        		path(node_name("startLocation").toString) = current;
		      		}		      
		    	}
		  	}
		  
			debugcount += 1;
			println();
		}
		
		println(path);
		println();
		println();
		println();
		println("**********PATH TAKEN**********");
		println(Map("distance" -> init_distance(finish).toInt, "path" -> compile_path(path, finish)));
		println("******************************");
		println();
		println();
		println();


	} //end_ dijkstra function




	  // FUNCTION: Main function
		def main (args: Array[String]) {

		//insert test case here
		val test0_map_edges = List( Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Mark's crib", "distance" -> 9),
								Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Greg's casa", "distance" -> 4),
								Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Matt's pad", "distance" -> 18),
								Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Brian's apartment", "distance" -> 8),
								Map("startLocation" -> "Brian's apartment", "endLocation" -> "Wesley's condo", "distance" -> 7),
								Map("startLocation" -> "Brian's apartment", "endLocation" -> "Cam's dwelling", "distance" -> 17),
								Map("startLocation" -> "Greg's casa", "endLocation" -> "Cam's dwelling", "distance" -> 13),
								Map("startLocation" -> "Greg's casa", "endLocation" -> "Mike's digs", "distance" -> 19),
								Map("startLocation" -> "Greg's casa", "endLocation" -> "Matt's pad", "distance" -> 14),
								Map("startLocation" -> "Wesley's condo", "endLocation" -> "Kirk's farm", "distance" -> 10),
								Map("startLocation" -> "Wesley's condo", "endLocation" -> "Nathan's flat", "distance" -> 11),
								Map("startLocation" -> "Wesley's condo", "endLocation" -> "Bryce's den", "distance" -> 6),
								Map("startLocation" -> "Matt's pad", "endLocation" -> "Mark's crib", "distance" -> 19),
								Map("startLocation" -> "Matt's pad", "endLocation" -> "Nathan's flat", "distance" -> 15),
								Map("startLocation" -> "Matt's pad", "endLocation" -> "Craig's haunt", "distance" -> 14),
								Map("startLocation" -> "Mark's crib", "endLocation" -> "Kirk's farm", "distance" -> 9),
								Map("startLocation" -> "Mark's crib", "endLocation" -> "Nathan's flat", "distance" -> 12),
								Map("startLocation" -> "Bryce's den", "endLocation" -> "Craig's haunt", "distance" -> 10),
								Map("startLocation" -> "Bryce's den", "endLocation" -> "Mike's digs", "distance" -> 9),
								Map("startLocation" -> "Mike's digs", "endLocation" -> "Cam's dwelling", "distance" -> 20),
								Map("startLocation" -> "Mike's digs", "endLocation" -> "Nathan's flat", "distance" -> 12),
								Map("startLocation" -> "Cam's dwelling", "endLocation" -> "Craig's haunt", "distance" -> 18),
								Map("startLocation" -> "Nathan's flat", "endLocation" -> "Kirk's farm", "distance" -> 3));

		val test1_map_edges = List( Map("startLocation" -> "A", "endLocation" -> "B", "distance" -> 24),
								Map("startLocation" -> "A", "endLocation" -> "C", "distance" -> 3),
								Map("startLocation" -> "A", "endLocation" -> "D", "distance" -> 20),
								Map("startLocation" -> "C", "endLocation" -> "D", "distance" -> 12));
		
		val test2_map_edges = List( Map("startLocation" -> "A", "endLocation" -> "B", "distance" -> 2),
								Map("startLocation" -> "A", "endLocation" -> "C", "distance" -> 3),
								Map("startLocation" -> "B", "endLocation" -> "C", "distance" -> 5),
								Map("startLocation" -> "B", "endLocation" -> "D", "distance" -> 4),
								Map("startLocation" -> "C", "endLocation" -> "D", "distance" -> 2),
								Map("startLocation" -> "D", "endLocation" -> "A", "distance" -> 1));

		val test3_map_edges = List( Map("startLocation" -> "A", "endLocation" -> "B", "distance" -> 14),
								Map("startLocation" -> "A", "endLocation" -> "C", "distance" -> 10),
								Map("startLocation" -> "A", "endLocation" -> "F", "distance" -> 7),
								Map("startLocation" -> "B", "endLocation" -> "F", "distance" -> 2),
								Map("startLocation" -> "B", "endLocation" -> "D", "distance" -> 8),
								Map("startLocation" -> "C", "endLocation" -> "F", "distance" -> 13),
								Map("startLocation" -> "C", "endLocation" -> "E", "distance" -> 18),
								Map("startLocation" -> "D", "endLocation" -> "E", "distance" -> 6),
								Map("startLocation" -> "E", "endLocation" -> "F", "distance" -> 11));
		  

		
		//solves and prints

		//test0, given test case
		println("given test case:");
		dijkstra_alg("Kruthika's abode", "Craig's haunt", test0_map_edges);
		dijkstra_alg("Kruthika's abode", "Kruthika's abode", test0_map_edges);
		dijkstra_alg("Craig's haunt", "Kruthika's abode", test0_map_edges);


		//test1 tests, has sink node
	  	println("test cases for test1:");
	  	dijkstra_alg("A", "D", test1_map_edges);
	  	dijkstra_alg("A", "B", test1_map_edges);
	  	dijkstra_alg("A", "C", test1_map_edges);
	    dijkstra_alg("C", "D", test1_map_edges);
	    dijkstra_alg("D", "D", test1_map_edges);

	    //test2 tests
	    println("test cases for test2:");
	    dijkstra_alg("A", "D", test2_map_edges);
	    dijkstra_alg("B", "D", test2_map_edges);
	    dijkstra_alg("B", "C", test2_map_edges);
	    dijkstra_alg("A", "B", test2_map_edges);
	    dijkstra_alg("B", "A", test2_map_edges);
			//dijkstra_alg("Kruthika's abode", "A", test2_map_edges); //THIS IS A TEST CASE THAT WILL THROW AN Exit(1)
	    
			//test3 tests, short 'early' route
	    println("test cases for test3:");
	    dijkstra_alg("A", "D", test3_map_edges);






	}//main





}//obj dolldelivery


