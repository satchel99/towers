import scala.io.Source
import towers.model.GridLocation
import towers.model.physics.PhysicsVector
import towers.model.graphs.BFS;
import towers.model.graphs.Graph;
import play.api.libs.json.{JsValue, Json};
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Queue;
import scala.collection.immutable.Set;


object NewTest {
    
    def process(start : Int, current : Int, path : ListBuffer[Int]) : ListBuffer[Int] =
     {
         
        if(current == start) {
            return path;
        }
        //start at last node.
        //keep working backwards.
        for(i <- heap) {
            for((k, v) <- i) {
                for(b <- v) {
                    for((j,u) <- b) {
                        if(u == current) {
                            var newPath : ListBuffer[Int] = j +: path
                            return process(start, j, newPath)
                        }
                    }
                }  
            }
        }
         
        return path
    }
    
    var heap : ListBuffer[Map[Int, ListBuffer[Map[Int, Int]]]] = ListBuffer()
    
    def bfs[A](graph: Graph[A], startID: Int, endID : Int): Unit = {

    var explored: Set[Int] = Set(startID)
    var exploredMap: ListBuffer[Map[Int, Int]] = ListBuffer[Map[Int, Int]]()

    val toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(startID)

    while (!toExplore.isEmpty) {
      val nodeToExplore : Int = toExplore.dequeue()
      for (node <- graph.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)){
          println("exploring: " + graph.nodes(node))
          toExplore.enqueue(node)
          explored = explored + node
          exploredMap = exploredMap :+ Map(nodeToExplore -> node)
        }
      }
    }
    var newmap : Map[Int, ListBuffer[Map[Int, Int]]] = Map(startID -> exploredMap)
    heap = heap :+ newmap
    if(!explored.contains(endID)) {
        for(v <- explored) {
            bfs(graph, v, endID)
        }
    }
    }
    
    def hWallBtw(walls : ListBuffer[GridLocation], start : GridLocation, end : GridLocation) : Int = {
        for(wall <- walls) {
            if(isBetweenY(start, end, wall.y)) {
                return wall.y;
            }
        }
        return -1
    }
    def vWallBtw(walls : ListBuffer[GridLocation], start : GridLocation, end : GridLocation) : Int = {
        for(wall <- walls) {
            if(isBetweenX(start, end, wall.x)) {
                return wall.x;
            }
        }
        return -1
    }
    
    def getWallsV(walls : ListBuffer[GridLocation], x : Int) : ListBuffer[GridLocation] = {
        var returnWalls : ListBuffer[GridLocation] = ListBuffer()
        for(wall <- walls) {
            if(wall.x == x) {
                returnWalls = returnWalls :+ wall 
            }
        }
        return returnWalls
    }
    
    def getWallsH(walls : ListBuffer[GridLocation], x : Int) : ListBuffer[GridLocation] = {
        var returnWalls : ListBuffer[GridLocation] = ListBuffer()
        for(wall <- walls) {
            if(wall.x == x) {
                returnWalls = returnWalls :+ wall 
            }
        }
        return returnWalls
    }
    
    
    
    
    def isBetweenX(start : GridLocation, end : GridLocation, x : Int)  : Boolean = {
        if((start.x < x) && (x < end.x)) {
            return true
        }
        if((end.x < x) && (x < start.x)) {
            return true
        }
        return false
    }
    
    def isBetweenY(start : GridLocation, end : GridLocation, y : Int)  : Boolean = {
        if((start.y < y) && (y < end.y)) {
            return true
        }
        if((end.y < y) && (y < start.y)) {
            return true
        }
        return false
    }
    
  
    def makePath(start : GridLocation, end : GridLocation, walls : ListBuffer[GridLocation], gridSize : Map[String, Int]) : List[GridLocation]  = {
        
         heap = ListBuffer()
        
          var graph : Graph[GridLocation] = new Graph()
          var gridWith : Int = gridSize("x");
          var gridHeight : Int = gridSize("y");
          //add nodes
          var count : Int = 0;
          for(i <- 0 to gridWith-1) {
                for(j <- 0 to gridHeight-1) {
                   if(!containsLoc(walls, new GridLocation(i, j))) {
                       graph.addNode(count, new GridLocation(i, j));
                       count = count + 1;
                   }
                }  
         }

         //add edges.
         for((k, v) <- graph.nodes) {
             var up : Int = getIndex(graph.nodes, v.x, v.y - 1);
             var down : Int = getIndex(graph.nodes, v.x, v.y + 1);
             var left : Int = getIndex(graph.nodes, v.x - 1, v.y);
             var right : Int = getIndex(graph.nodes, v.x + 1, v.y);
             var allDir : List[Int] = List(up, down, left, right);

             for(dir <- allDir) {
                 if(dir != -1) {
                     graph.addEdge(k, dir);
                 }
             }
         }
        
        
        bfs(graph, getIndex(graph.nodes, start.x, start.y), getIndex(graph.nodes, end.x, end.y))
        
        println("trying bfs")
        var result : ListBuffer[Int] = process(getIndex(graph.nodes, start.x, start.y), getIndex(graph.nodes, end.x, end.y), new ListBuffer[Int]())

        var path : List[GridLocation] = List()

        //find shortest path
        if((start.x == end.x) && ((start.y == end.y))) {
            path = List(start)
        }
        else {
            var retL : ListBuffer[GridLocation] = ListBuffer()
            for(node <- result) {
                retL = retL :+ graph.nodes(node)
            }
            path = retL.toList
        }
        return path
        
    }
    
    
    def existsAt(walls : ListBuffer[GridLocation], x : Int, y : Int) : Boolean = {
        for(v <- walls) {
            if((v.y == y) && (v.x == x)) {
                return true;
            }
        }
        return false;
    }
    
    def getHoles(walls : ListBuffer[GridLocation], boardWidth : Int, x : Int) : ListBuffer[Int] =  {
        var holes : ListBuffer[Int] = ListBuffer();
        for(i <- 0 to boardWidth - 1) {
            if(!existsAt(walls, x, i)) {
                holes = i +: holes
            }
        }
        return holes
    }
    
    def closestHole(playerLoc : GridLocation, holes : ListBuffer[Int]) : Int = {
        var min : Int = holes(0)
        for(v <- holes) {
            if(Math.abs(playerLoc.y - v) < Math.abs(playerLoc.y - min)) {
                min = v;
            }
        }
        return min;
     }
    
    def getPath(jsonGameState: String): List[GridLocation] = {
        
      var id : String = "1"
      println("-----------------------------------")
      println(jsonGameState)
      println("-----------------------------------")
      
      
      //             Get Player Location
      var playerLoc : GridLocation = null;
      val playerL: List[Map[String, JsValue]] = (Json.parse(jsonGameState) \ "players").as[List[Map[String, JsValue]]]
      var playersList : ListBuffer[List[JsValue]] = new ListBuffer();
        for(v <- playerL) {
            val x : JsValue = v("x")
            val y : JsValue = v("y")
            val vx : JsValue = v("v_x")
            val vy : JsValue = v("v_y")
            val id : JsValue = v("id")
            val tempList : List[JsValue] = List(x, y, vx, vy, id)
            playersList = tempList +: playersList
        }
      for(play <- playersList) {
          var idStr : String = play(4).as[String];
          if(idStr == id) {
              playerLoc = new GridLocation(play(0).as[Double].toInt, play(1).as[Double].toInt);
          }
      }
      //             Get Grid With and Height
      var gridSize: Map[String, Int] = (Json.parse(jsonGameState) \ "gridSize").as[Map[String, Int]]
      
      //             Get List of Walls.
      var walls : ListBuffer[GridLocation] = ListBuffer();
      var wallsJson : List[Map[String, JsValue]] = (Json.parse(jsonGameState) \ "walls").as[List[Map[String, JsValue]]]
      
      for(v <- wallsJson) {
            val x : JsValue = v("x")
            val y : JsValue = v("y")
            val xI : Int = x.as[Int];
            val yI : Int = y.as[Int];
            walls = new GridLocation(xI, yI) +: walls
        }
      
      //             Get Enemy Base Location.
      var base: Map[String, Int] = (Json.parse(jsonGameState) \ "base").as[Map[String, Int]]
      var baseLoc : GridLocation = new GridLocation(base("x"), base("y"))
      
        
    
      return makePath(playerLoc, baseLoc, walls, gridSize).toList
      
   
    }
    
    def containsLoc(walls: ListBuffer[GridLocation], tile: GridLocation) : Boolean = {
      for(v <- walls) {
          if((v.x == tile.x) && (v.y == tile.y)) {
              return true;
          }
      }
      return false;
  }
    
  def getIndex(board : Map[Int, GridLocation], x : Int, y : Int) : Int = {
      for((k, v) <- board) {
          if((v.x == x) && (v.y == y)) {
              return k
          }
      }
      return -1;
  }
//find first whole in the wall
//go there
//find path from hole to base
    def main(args : Array[String]) : Unit = {
        var json : String = "";
        val filename = "input.txt"
        for (line <- Source.fromFile(filename).getLines) {
            json = json + line
        }
        println(getPath(json))
        var wallLocations : List[GridLocation] = List(
              new GridLocation(10,0),
              new GridLocation(10,1),
              new GridLocation(10,2),
              new GridLocation(10,6),
              new GridLocation(10,7),
              new GridLocation(10,8)
            )
        //println(getHoles(wallLocations.to[ListBuffer], 9))
        //println(closestHole(new GridLocation(2, 1), getHoles(wallLocations.to[ListBuffer], 9)))
    }

}

