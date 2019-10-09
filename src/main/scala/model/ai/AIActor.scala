package towers.model.ai

import akka.actor.{Actor, ActorRef}
import towers.model.physics.PhysicsVector
import towers.model.{GameState, MovePlayer, StopPlayer}

class AIActor(gameActor: ActorRef, id: String) extends Actor {

  val aiPlayer = new AIPlayer(id)

  override def receive: Receive = {
    case gs:GameState =>
      val direction: PhysicsVector = aiPlayer.computeMovement(gs.gameState)
      if(direction.x == 0 && direction.y == 0){
        gameActor ! StopPlayer(id)
      }else{
        gameActor ! MovePlayer(id, direction.x, direction.y)
      }
  }

}
