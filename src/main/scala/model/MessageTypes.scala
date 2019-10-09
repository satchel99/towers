package towers.model


// Received by Multiple Types
case object SendGameState
case class GameState(gameState: String)


// Received by GameActor
case object UpdateGame
case object TowersFire
case class AddPlayer(username: String)
case class RemovePlayer(username: String)
case class MovePlayer(username: String, x: Double, y:Double)
case class StopPlayer(username: String)
case class AddProjectile(x: Double, y:Double, z:Double, xVelocity:Double, yVelocity:Double, zVelocity:Double)
case class LoadLevel(levelNumber: Int)


// Received by TowerActor
case object Fire


// Received by AIController
case object Update