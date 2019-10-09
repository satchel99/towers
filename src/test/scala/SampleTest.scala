package towers.model.ai

import org.scalatest._
import towers.model.GridLocation

class SampleTest extends FunSuite {


  test("test path modified level 0") {

    val jsonGameState = "{\"baseHealth\":20," +
      "\"players\":[{\"x\":0.9,\"v_y\":0,\"y\":0.1,\"id\":\"0\",\"v_x\":4}]," +
      "\"projectiles\":[]," +
      "\"base\":{\"x\":24,\"y\":0}," +
      "\"towers\":[{\"x\":22,\"y\":6},{\"x\":20,\"y\":2},{\"x\":15,\"y\":3},{\"x\":15,\"y\":4},{\"x\":15,\"y\":5}]," +
      "\"walls\":[{\"x\":10,\"y\":8},{\"x\":10,\"y\":7},{\"x\":10,\"y\":6},{\"x\":10,\"y\":2},{\"x\":10,\"y\":1},{\"x\":10,\"y\":0}]," +
      "\"start\":{\"x\":0,\"y\":0}," +
      "\"gridSize\":{\"x\":25,\"y\":9}," +
      "\"maxBaseHealth\":20}"


    val wallLocations = List(
      new GridLocation(10,0),
      new GridLocation(10,1),
      new GridLocation(10,2),
      new GridLocation(10,6),
      new GridLocation(10,7),
      new GridLocation(10,8)
    )


    val computedPath = new AIPlayer("0").getPath(jsonGameState)

    println("Your Path: " + computedPath)


    val shortestPath = List(
      new GridLocation(0, 0),
      new GridLocation(0, 1),
      new GridLocation(1, 1),
      new GridLocation(2, 1),
      new GridLocation(3, 1),
      new GridLocation(4, 1),
      new GridLocation(5, 1),
      new GridLocation(6, 1),
      new GridLocation(7, 1),
      new GridLocation(7, 2),
      new GridLocation(8, 2),
      new GridLocation(9, 2),
      new GridLocation(9, 3),
      new GridLocation(10, 3),
      new GridLocation(11, 3),
      new GridLocation(12, 3),
      new GridLocation(13, 3),
      new GridLocation(14, 3),
      new GridLocation(15, 3),
      new GridLocation(16, 3),
      new GridLocation(17, 3),
      new GridLocation(18, 3),
      new GridLocation(19, 3),
      new GridLocation(20, 3),
      new GridLocation(21, 3),
      new GridLocation(22, 3),
      new GridLocation(23, 3),
      new GridLocation(24, 3),
      new GridLocation(24, 2),
      new GridLocation(24, 1),
      new GridLocation(24, 0)
    )

    assert(computedPath.size == shortestPath.size)

    computedPath.foreach(location => assert(!wallLocations.contains(location)))

    var previous = computedPath.head
    for(location <- computedPath.drop(1)){
      val l1Distance = Math.abs(location.x - previous.x) + Math.abs(location.y - previous.y)
      assert(l1Distance == 1)
      previous = location
    }

  }

}