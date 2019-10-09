package towers.model.game_objects

import play.api.libs.json.{JsValue, Json}
import towers.model.genetics.genes.Gene
import towers.model.physics.PhysicsVector
import scala.collection.mutable.ListBuffer;

class DodgeBallTower(val x: Int, val y: Int) extends GameObject {

  // The height at which projectiles are fired
  val height = 3.0

  // Towers can only fire at players closer than this distance from the tower
  val sightRange = 5.0

  // The magnitude of the velocity at which projectiles are fired
  val projectileVelocity = 5.0
    
  def computeDistance(somePlayer : List[JsValue]) : Double = {
      var x : Double = somePlayer(0).as[Double]
      var y : Double = somePlayer(1).as[Double]
      return new PhysicsVector(x, y).distance2d(new PhysicsVector(this.x + 0.5, this.y + 0.5));
  }
    
 def getTheta(somePlayer : List[JsValue]) : Double = {
      var x : Double = somePlayer(0).as[Double]
      var y : Double = somePlayer(1).as[Double]
      var vx : Double = somePlayer(2).as[Double]
      var vy : Double = somePlayer(3).as[Double]
      return x
     
  }
    
  def findMin(players : ListBuffer[List[JsValue]]) : Int = {
      var min : Int = -1;
      var minV : Double = 0.0;
      for(i <- 0 to players.length-1) {
          val someDist : Double = computeDistance(players(i));
          if(min == -1) {
              minV = someDist
              min = i;
          }
          else {
              if(someDist < minV) {
                  minV = someDist;
                  min = i;
              }
          }
      }
      return min
  }
  
  def sortPlayers(players : ListBuffer[List[JsValue]]) : ListBuffer[List[JsValue]]  = {
        var newList : ListBuffer[List[JsValue]] = ListBuffer[List[JsValue]]();
        for(i <- 0 to players.length-1) {
            var index : Int = findMin(players)
            newList =  newList :+ players(index)
            players.remove(index)
        }
        return newList
    }


  def fire(jsonGameState: String): List[Projectile] = {
    println("==========")
    var playersList : ListBuffer[List[JsValue]] = new ListBuffer();
    val playerL: List[Map[String, JsValue]] = (Json.parse(jsonGameState) \ "players").as[List[Map[String, JsValue]]]
    for(v <- playerL) {
        val x : JsValue = v("x")
        val y : JsValue = v("y")
        val vx : JsValue = v("v_x")
        val vy : JsValue = v("v_y")
        val id : JsValue = v("id")
        val tempList : List[JsValue] = List(x, y, vx, vy, id)
        playersList = tempList +: playersList
    }
    var sorted : ListBuffer[List[JsValue]] = sortPlayers(playersList);
    for(v <- sorted) {
        println(computeDistance(v))
    }
    println("==========")
    if(sorted.length == 0) {
        return List()
    }
    val chosen : List[JsValue] = sorted(0);
    if(computeDistance(chosen) <= sightRange) {
        print("here")
        val currLoc = new PhysicsVector(chosen(0).as[Double], chosen(1).as[Double])
        var newX : Double = currLoc.x - (this.x+0.5);
        var newY : Double = currLoc.y - (this.y+0.5);
        var normalDirection : PhysicsVector = new PhysicsVector(newX, newY).normal2d()
        println(normalDirection)
        println(newX + " " + newY)
        println(normalDirection.x + " " + normalDirection.y)
        var proj : Projectile = new Projectile(new PhysicsVector(this.x + 0.5, this.y + 0.5, height), new PhysicsVector(normalDirection.x*projectileVelocity, normalDirection.y*projectileVelocity));
        print(Math.pow(proj.velocity.x, 2.0) + Math.pow(proj.velocity.y, 2.0))
        return List(proj)
    }
    //val username = (message \ "username").as[String]
    //val messageType = (message \ "action").as[String]
    List()
  }


  def aimFire(jsonGameState: String): List[Projectile] = {
    // TODO: Objective 2
    println("==========")
    var playersList : ListBuffer[List[JsValue]] = new ListBuffer();
    val playerL: List[Map[String, JsValue]] = (Json.parse(jsonGameState) \ "players").as[List[Map[String, JsValue]]]
    for(v <- playerL) {
        val x : JsValue = v("x")
        val y : JsValue = v("y")
        val vx : JsValue = v("v_x")
        val vy : JsValue = v("v_y")
        val id : JsValue = v("id")
        val tempList : List[JsValue] = List(x, y, vx, vy, id)
        playersList = tempList +: playersList
    }
    var sorted : ListBuffer[List[JsValue]] = sortPlayers(playersList);
    for(v <- sorted) {
        println(computeDistance(v))
    }
    println("==========")
    if(sorted.length == 0) {
        return List()
    }
    val chosen : List[JsValue] = sorted(0);
    if(computeDistance(chosen) <= sightRange) {
        print("here")
        val currLoc = new PhysicsVector(chosen(0).as[Double], chosen(1).as[Double])
        val currVol = new PhysicsVector(chosen(2).as[Double], chosen(3).as[Double])
        var xP : Double = currLoc.x + currVol.x; 
        var yP : Double = currLoc.y + currVol.y;
        var dispY : Double = xP - (this.x + 0.5)
        var dispX : Double = xP - (this.x + 0.5)
        var vX : Double = dispX;
        var vY : Double = dispY + 5;
        var proj : Projectile = new Projectile(new PhysicsVector(this.x + 0.5, this.y + 0.5, height), new PhysicsVector(vX, vY));
        return List(proj)
    }
    //val username = (message \ "username").as[String]
    //val messageType = (message \ "action").as[String]
    List()
  }


  // Suggested Genetic Algorithm setup
  def getFitnessFunction(targetPlayer: Player): PhysicsVector => Double = {
    null
  }

  def vectorIncubator(genes: List[Gene]): PhysicsVector = {
    null
  }

}
