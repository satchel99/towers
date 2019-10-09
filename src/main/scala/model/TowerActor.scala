package towers.model

import akka.actor.{Actor, ActorRef}
import towers.model.game_objects.DodgeBallTower

class TowerActor(gameActor: ActorRef, x: Int, y: Int) extends Actor {

  import context.dispatcher
  import scala.concurrent.duration._

  val tower: DodgeBallTower = new DodgeBallTower(x, y)

  // start firing as soon as tower is created
  gameActor ! SendGameState

  var gameState: String = ""

  override def receive: Receive = {
    case Fire =>
      if(gameState != "") {
        val projectiles = tower.fire(gameState)
//        val projectiles = tower.aimFire(gameState)

        projectiles.foreach(proj => gameActor ! AddProjectile(proj.location.x, proj.location.y, proj.location.z, proj.velocity.x, proj.velocity.y, proj.velocity.z))
      }

      // fire again in 1 second
      context.system.scheduler.scheduleOnce(1000.milliseconds, gameActor, SendGameState)

    case gs: GameState =>
      gameState = gs.gameState
      self ! Fire
  }


}
