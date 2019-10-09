package towers.model.ai

import towers.model.GridLocation
import towers.model.physics.PhysicsVector
import towers.model.graphs.BFS;
import towers.model.graphs.Graph;
import play.api.libs.json.{JsValue, Json};
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Queue;
import scala.collection.immutable.Set;
class AIPlayer(id: String) {
    
     var heap : ListBuffer[Map[Int, ListBuffer[Map[Int, Int]]]] = ListBuffer()


  def computeMovement(jsonGameState: String): PhysicsVector = {
    // Do not edit this method. It will not be called during grading
    var path = getPath(jsonGameState)
    path = smoothPath(jsonGameState, path)
    pathToDirection(jsonGameState, path)
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
def getWallX(walls : ListBuffer[GridLocation], y : Int) : Int = {
        for(v <- walls) {
            if(v.y == y) {
                return v.x;
            }
        }
        return -1;
    }
    
    def existsAt(walls : ListBuffer[GridLocation], y : Int) : Boolean = {
        for(v <- walls) {
            if(v.y == y) {
                return true;
            }
        }
        return false;
    }
    
    def getHoles(walls : ListBuffer[GridLocation], boardWidth : Int) : ListBuffer[Int] =  {
        var holes : ListBuffer[Int] = ListBuffer();
        for(i <- 0 to boardWidth - 1) {
            if(!existsAt(walls, i)) {
                holes = i +: holes
            }
        }
        println(walls)
        println(holes)
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
    
def makePathSmoothe(start : GridLocation, end : GridLocation, walls : ListBuffer[GridLocation], gridSize : Map[String, Int]) : List[GridLocation]  = {
        
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
        return path
        
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
        result = result :+ getIndex(graph.nodes, end.x, end.y)
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

 
def getPath(jsonGameState: String): List[GridLocation] = {
        
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


  def pathToDirection(jsonGameState: String, path: List[GridLocation]): PhysicsVector = {
    var playerLoc : GridLocation = null;
      var playerX : Double = 0.0;
       var playerY : Double = 0.0;
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
          if(idStr == this.id) {
              playerX = play(0).as[Double]
              playerY = play(1).as[Double]
              playerLoc = new GridLocation(play(0).as[Double].toInt, play(1).as[Double].toInt);
          }
      }
    // TODO
    if(path.length == 1) {
        val currLoc = new PhysicsVector(playerLoc.x, playerLoc.y)
        var centerX : Double = path(0).x.toInt + 0.5
        var centerY : Double = path(0).y.toInt + 0.5
        var newX : Double = centerX - (playerX);
        var newY : Double = centerY - (playerY);
        var normalDirection : PhysicsVector = new PhysicsVector(newX, newY).normal2d()
        println("+" + normalDirection)
        println(currLoc)
        println(playerX + " " + playerY)
        println(path(0))
        return normalDirection;
    }
    else if(path.length > 1) {
        println("//////////")
        println("Current location " + playerX + " " + playerY)
        println("next tile " + path(1))
        val currLoc = new PhysicsVector(playerLoc.x.toInt + 0.5, playerLoc.y.toInt + 0.5)
        var centerX : Double = path(1).x.toInt + 0.5
        var centerY : Double = path(1).y.toInt + 0.5
        var newX : Double = centerX - (playerX);
        var newY : Double = centerY - (playerY);
        var normalDirection : PhysicsVector = new PhysicsVector(newX, newY).normal2d()
        println("+" + normalDirection)
        println(currLoc)
        println(playerLoc)
        println(path(1))
        return normalDirection;
    }
    new PhysicsVector(Math.random() - 0.5, Math.random() - 0.5)
  }


  def smoothPath(jsonGameState: String, path: List[GridLocation]): List[GridLocation] = {
        
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
      
        
    
      return makeSmoothePath(playerLoc, baseLoc, walls, gridSize).toList
      
   
    }
  def makeSmoothePath(start : GridLocation, end : GridLocation, walls : ListBuffer[GridLocation], gridSize : Map[String, Int]) : List[GridLocation]  = {
        
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
             var topLeft : Int = getIndex(graph.nodes, v.x + 1, v.y-1);
             var bottomRight : Int = getIndex(graph.nodes, v.x + 1, v.y+1);
             var bottomLeft : Int = getIndex(graph.nodes, v.x - 1, v.y+1);
             var topRight : Int = getIndex(graph.nodes, v.x - 1, v.y-1);
             
             if((start.x < end.x) && (start.y < end.y)) {
                 allDir = bottomRight +: allDir
             }
             if((start.x > end.x) && (start.y < end.y)) {
                 allDir = bottomLeft +: allDir
             }
             if((start.x > end.x) && (start.y > end.y)) {
                 allDir = topRight +: allDir
             }
            if((start.x < end.x) && (start.y > end.y)) {
                 allDir = topLeft +: allDir
             }

             for(dir <- allDir) {
                 if(dir != -1) {
                     graph.addEdge(k, dir);
                 }
             }
         }
        
        
        bfs(graph, getIndex(graph.nodes, start.x, start.y), getIndex(graph.nodes, end.x, end.y))
        
        println("trying bfs")
        var result : ListBuffer[Int] = process(getIndex(graph.nodes, start.x, start.y), getIndex(graph.nodes, end.x, end.y), new ListBuffer[Int]())
        result = result :+ getIndex(graph.nodes, end.x, end.y)
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

}
