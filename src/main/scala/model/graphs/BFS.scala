package towers.model.graphs

import scala.collection.mutable.Queue;
import scala.collection.immutable.Set;
import scala.collection.mutable.ListBuffer;

object BFS {

  def bfs[A](graph: Graph[A], startID: Int, secondID : Int): Unit = {
    println("here")
    var paths: ListBuffer[ListBuffer[Int]] = ListBuffer()

    var explored: Set[Int] = Set(startID)

    val toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(startID)

    while (!toExplore.isEmpty) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- graph.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)){
          println("exploring: " + graph.nodes(node))
          toExplore.enqueue(node)
          explored = explored + node
          if(node == secondID) {
              var newL : ListBuffer[Int] = ListBuffer[Int]()
              while(!explored.isEmpty){
                  newL = explored.head +: newL
                  explored = explored.tail
              }
              println(newL)
              paths = newL +: paths
          }
          else {
              bfs(graph, node, secondID);
          }
        }
      }
    }
    println("here")
    println(paths)
  }

}
