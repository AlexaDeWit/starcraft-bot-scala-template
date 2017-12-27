import bwapi.{Unit => ScUnit, _}
import bwta.BWTA
import scalaz._
import Scalaz._
import BehaviourFunctions._
import scala.collection.JavaConverters._

object BWBotListener {
  def main(args: Array[String]): Unit =
    new BWBotListener().run()
}

class BWBotListener extends DefaultBWListener {
  val mirror = new Mirror()
  var game: Game = _
  var self: Player = _

  def run(): Unit = {
    mirror.getModule.setEventListener(this)
    mirror.startGame()
  }

  override def onUnitCreate(unit: ScUnit): Unit = {
    System.out.println("New unit " + unit.getType)
  }

  override def onStart(): Unit = {
    game = mirror.getGame
    self = game.self()

    //Use BWTA to analyze map
    //This may take a few minutes if the map is processed first time!
    System.out.println("Analyzing map...")
    BWTA.readMap()
    BWTA.analyze()

    game.enableFlag(1) //Enables control of AI
    game.setLocalSpeed(5) //20 is tournament speed

    System.out.println("Map data ready")
  }

  override def onFrame(): Unit = {
    //game.setTextSize(10);
    game.drawTextScreen(10, 10, "Playing as " + self.getName + " - " + self.getRace)
    self.getUnits.asScala.filter(u => !isNonCombatUnit(u))

    trainWorkers(self).unsafePerformIO()
    buildSupplyStructures(game, self).unsafePerformIO()
    orders(self, game)
  }

  def packGameState: GameState = ???
  
  def unpackGameState: GameState = ???


}
