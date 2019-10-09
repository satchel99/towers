package towers.model.ai

import akka.actor.{Actor, ActorRef, Props}
import towers.model.{AddPlayer, GameState, SendGameState, Update}

class AIController(gameActor: ActorRef) extends Actor {

  val numberOfAIPlayers = 7

  var aiPlayers: Map[ActorRef, String] = Map()

  for (i <- 0 until numberOfAIPlayers) {
    aiPlayers += context.actorOf(Props(classOf[AIActor], gameActor, i.toString)) -> i.toString
    gameActor ! AddPlayer(i.toString)
  }

  override def receive: Receive = {
    case Update => gameActor ! SendGameState
    case gs: GameState => aiPlayers.keys.foreach(ai => ai ! gs)
  }

}
