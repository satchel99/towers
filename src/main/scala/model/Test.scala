import scala.io.Source
import towers.model.GridLocation
import towers.model.physics.PhysicsVector
import towers.model.graphs.BFS;
import towers.model.graphs.Graph;
import play.api.libs.json.{JsValue, Json};
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Queue;
import scala.collection.immutable.Set;

object Test {
    
    def bfs[A](graph: Graph[A], startID: Int): Unit = {

        var explored: Set[Int] = Set(startID)

        val toExplore: Queue[Int] = new Queue()
        toExplore.enqueue(startID)

        while (!toExplore.isEmpty) {
          val nodeToExplore = toExplore.dequeue()
          for (node <- graph.adjacencyList(nodeToExplore)) {
            if(!explored.contains(node)){
              toExplore.enqueue(node)
              explored = explored + node
            }
          }
        }
        
        println("begin explored ")
        for(i <- explored) {
            print(graph.nodes(i) + ",")
        }
        println(" end explored ")
      }
    
    
    
    
    
    
    def ifWall(start : GridLocation, end : GridLocation, walls : ListBuffer[GridLocation], gridWidth : Int) : GridLocation = {
        
        var problemWalls : ListBuffer[GridLocation] = ListBuffer()
        
        for(wall <- walls) {
            if(((start.y < wall.y) && (wall.y < end.y)) || ((end.y < wall.y) && (wall.y < start.y))){
                problemWalls = problemWalls :+ wall
            }
        }
        
        if(problemWalls.length == 0) {
            return new GridLocation(-1, -1)
        }
        
        var minY : Int = problemWalls(0).y
        
        for(wall <- problemWalls) {
            if(wall.y < minY) {
                minY = wall.y
            }
        }
        
        var holes : ListBuffer[GridLocation] = ListBuffer()
        for(i <- 0 to gridWidth -1) {
            if(existsAt(walls, i, minY)) {
                holes = holes :+ new GridLocation(i, minY)
            }
        }
        
        var min : GridLocation = holes(0)
        for(wall <- holes) {
            if(Math.sqrt(Math.pow((wall.x - start.x), 2) + Math.pow((wall.y - start.y), 2)) < Math.sqrt(Math.pow((min.x - start.x), 2) + Math.pow((min.y - start.y), 2))) {
                min = wall;
            }
            
        }
        return min
    }
    
    def getWallXList(walls : ListBuffer[GridLocation]) : ListBuffer[Int] = {
        var xl : ListBuffer[Int] = ListBuffer();
            for(v <- walls) {
                if(!walls.contains(v.x)) {
                    xl = xl :+ v.x
                }
            }
        return xl;
    }
    
    
    def isBetween(start : GridLocation, end : GridLocation, x : Int)  : Boolean = {
        if((start.x < x) && (x < end.x)) {
            return true
        }
        if((end.x < x) && (x < start.x)) {
            return true
        }
        return false
    }
    
    def allWallsOnX(walls : ListBuffer[GridLocation]) : ListBuffer[Int] = {
        var xl : ListBuffer[Int] = ListBuffer();
            for(v <- walls) {
                if(!walls.contains(v.x)) {
                    xl = xl :+ v.x
                }
            }
        return xl;
    }
    
    
    def makePath(start : GridLocation, end : GridLocation, walls : ListBuffer[GridLocation], gridSize : Map[String, Int]) : List[GridLocation]  = {
        
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
             var allDir : ListBuffer[Int] = ListBuffer();

             if(start.x < end.x) {
                 allDir = right +: allDir
             }
             else {
                 allDir = left +: allDir
             }
             if(start.y < end.y) {
                 allDir = down +: allDir
             }
             else {
                 allDir = up +: allDir
             }

             for(dir <- allDir) {
                 if(dir != -1) {
                     graph.addEdge(k, dir);
                 }
             }
         }

        var path : List[GridLocation] = List()

        //find shortest path
        if((start.x == end.x) && ((start.y == end.y))) {
            path = List(start)
        }
        else {
            var result : ListBuffer[Int] = graph.shortestPath(getIndex(graph.nodes, start.x, start.y),getIndex(graph.nodes, end.x, end.y))
            var retL : ListBuffer[GridLocation] = ListBuffer()
            for(node <- result) {
                retL = retL :+ graph.nodes(node)
            }
            path = retL.toList
        }
        println("------------------")
        bfs(graph, getIndex(graph.nodes, start.x, start.y))
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
      
      var gridWith : Int = gridSize("x");
      var gridHeight : Int = gridSize("y");
        
    /*   DELETE LATER   */
    
    var wallLocations : List[GridLocation] = List(
          new GridLocation(0,14),
          new GridLocation(0,15),
          new GridLocation(1,14),
          new GridLocation(1,15),
          new GridLocation(2,14),
          new GridLocation(2,15),
          new GridLocation(3,14),
          new GridLocation(3,15),
          new GridLocation(4,14),
          new GridLocation(4,15),
          new GridLocation(5,14),
          new GridLocation(5,15),
          new GridLocation(6,14),
          new GridLocation(6,15),



          new GridLocation(4, 3),
          new GridLocation(4, 4),
          new GridLocation(4, 5),
          new GridLocation(4, 6),
          new GridLocation(4, 7),
          new GridLocation(4, 8),
          new GridLocation(4, 9),


          new GridLocation(9, 3),
          new GridLocation(9, 4),
          new GridLocation(9, 5),
          new GridLocation(9, 6),
          new GridLocation(9, 7),
          new GridLocation(9, 8),
          new GridLocation(9, 9),


          new GridLocation(5, 9),
          new GridLocation(6, 9),
          new GridLocation(7, 9),
          new GridLocation(8, 9)
        );
        
        walls = wallLocations.to[ListBuffer]
        playerLoc = new GridLocation(1, 18)
        
        gridWith = 10
        gridHeight = 20
        
        gridSize = Map("x" -> 10, "y" -> 20)

        baseLoc = new GridLocation(6,7)
        base = Map("x" -> 6, "y" -> 7)
        makePath(playerLoc, baseLoc, walls, gridSize)
    
    /*   DELETE LATER   */
        
    var allPaths : ListBuffer[List[GridLocation]] = ListBuffer()
    var allWalls : ListBuffer[Int] = getWallXList(walls)
    
    var currentLoc : GridLocation = new GridLocation(playerLoc.x, playerLoc.y)
        
    for(xWall <- allWalls) {
         if(isBetween(currentLoc, baseLoc, xWall)) {
             println("here")
             var closestHoleY : Int = closestHole(currentLoc, getHoles(walls, gridHeight, xWall))
             var closestHoleX : Int = xWall
             println("--- " + closestHoleX + " " + closestHoleY)
                          
             var pathToHoles : List[GridLocation] = makePath(currentLoc, new GridLocation(closestHoleX, closestHoleY), walls, gridSize);
             
             if(pathToHoles.length == 0) {
                 println("problem")
                 println(currentLoc)
                 println(new GridLocation(closestHoleX, closestHoleY))
                 println("-")
             }
             println(pathToHoles)
             
             allPaths = allPaths :+ pathToHoles
             currentLoc = new GridLocation(closestHoleX, closestHoleY)
             
         }
    }
        
    
    var newl : List[GridLocation] = List()
     println("pp")
     println(currentLoc)
     println(baseLoc)
     println(walls)
     newl = makePath(currentLoc, baseLoc, walls, gridSize);
     println(newl)
        
        
    //put them all together
    var finalL : ListBuffer[GridLocation] = ListBuffer()
    for(pathToHoles <- allPaths) {
        for(i <- 0 to pathToHoles.length-2) {
            finalL = finalL :+ pathToHoles(i)
        }
    }
    for(i <- 0 to newl.length-1) {
        finalL = finalL :+ newl(i)
    }
    return finalL.toList
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

