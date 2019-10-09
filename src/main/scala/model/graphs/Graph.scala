package towers.model.graphs
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Queue;
import scala.collection.immutable.Set;

class Graph[A] {

  var nodes: Map[Int, A] = Map()
  var adjacencyList: Map[Int, List[Int]] = Map()


  def addNode(index: Int, a: A): Unit = {
    nodes += index -> a
    adjacencyList += index -> List()
  }

  def addEdge(index1: Int, index2: Int): Unit = {
    adjacencyList += index1 -> (index2 :: adjacencyList(index1))
    //adjacencyList += index2 -> (index1 :: adjacencyList(index2))
  }

  def areConnected(index1: Int, index2: Int): Boolean = {
    // TODO: Does there exist a path between index1 and index2 in this graph?
    false
  }

  def distance(index1: Int, index2: Int): Int = {
    // TODO: Return the distance between index1 and index2 in this graph
    // You may assume that the two nodes are connected
    0
  }
   
def minList(input : ListBuffer[ListBuffer[Int]]) : ListBuffer[Int] = {
    if(input.size == 0) {
        return ListBuffer[Int]();
    }
    var min : ListBuffer[Int] = input(0)
    for(i <- input) {
        if(min.length == 0) {
            min = i
        }
        if(i.length > 0) {
            if(i.length < min.length) {
                min = i
            }
        } 
        
    }
    return min
}
    
def bfs[A](graph: Graph[A], startID: Int): Unit = {

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
        }
      }
    }
  }

def shortestPath(index1: Int, index2: Int): ListBuffer[Int] = {
    var toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(index1)
    var path : ListBuffer[Int] = new ListBuffer();
    path = index1 +: path
    return shortestPathHelper(index1, index2, path, toExplore)
}
def shortestPathHelper(index1: Int, index2: Int, traveled: ListBuffer[Int], toExplore: Queue[Int]): ListBuffer[Int] = {
    if(adjacencyList(index1).contains(index2)) {
        var newlTemp : ListBuffer[Int] = traveled :+ index2
        return newlTemp;
    }else {
        var fv : ListBuffer[ListBuffer[Int]] = ListBuffer()
        for(node <- adjacencyList(index1)) {
            if(!(toExplore.contains(node))) {
               toExplore.enqueue(node)
               var newQ : Queue[Int] = toExplore :+ node
               var newl : ListBuffer[Int] = traveled :+ node
               val result : ListBuffer[Int] = shortestPathHelper(node, index2, newl, toExplore)
               fv = result +: fv
            }
        }
        return minList(fv)
    }
}
}

//if there is a wall on the same x, and in between the next location.
