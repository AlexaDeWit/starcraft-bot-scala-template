import bwapi.{Unit => ScUnit, _}
import bwta.BWTA
import scalaz._
import Scalaz._
import BehaviourFunctions._
import scala.collection.JavaConverters._

object TestBot {
  def main(args: Array[String]): Unit =
    new TestBot().run()
}

class TestBot extends DefaultBWListener {
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
  def orders(player: Player, game: Game): Unit = {

    //Unit Orders

    //Workers
    trainWorkers(self).unsafePerformIO()
    workerMine(player, game)

    buildingPrioritizing(player, game, List())
    buildArmy(player, game)

  }

  def buildArmy(player: Player, game: Game): Unit = {
    if (player.getUnits.asScala.count(isArmyUnit) < 200) {
      trainUnit(UnitType.Terran_Barracks, UnitType.Terran_Marine)
    }
  }

  def getMyWorkers: List[ScUnit] = ???

  def designateABuilder(i: Int) = ???

  def buildStructure(game: Game, player: Player, unitType: UnitType) = ???

  def trainUnit(unitType: UnitType, unitType1: UnitType) = ???

  def isArmyUnit(unit: ScUnit) = !(unit.getType.isWorker || unit.getType.isBuilding || unit.getType.isNeutral || unit.getType.isAddon)


  def buildingPrioritizing(player: Player, game: Game, builders: List[ScUnit]): Unit = {
    if (builders.isEmpty) {
      designateABuilder(1)
    } else if (builders.size == 1 && getMyWorkers.size == 10) {
      designateABuilder(2)
    }
    val units = player.getUnits.asScala

    if (player.supplyTotal() - player.supplyUsed() <= 4) {
      buildStructure(game, player, UnitType.Protoss_Pylon)
    } else if (units.count(_.getType == UnitType.Protoss_Gateway) < 4) {
      buildStructure(game, player, UnitType.Protoss_Gateway)
    }
  }

  def workerMine(player: Player, game: Game) = {
    player.getUnits.asScala
      .filter(_.getType.isWorker)
      .filter(_.isIdle)
      .foreach { worker =>
        val closestMineral = game.neutral.getUnits.asScala
          .filter(_.getType.isMineralField)
          .map(mineral => (mineral.getDistance(worker), mineral))
          .sortBy(_._1)
          .map(_._2)
          .headOption

        closestMineral.foreach(worker.gather)
      }
  }
  override def onFrame(): Unit = {
    //game.setTextSize(10);
    game.drawTextScreen(10, 10, "Playing as " + self.getName + " - " + self.getRace)
    self.getUnits.asScala.filter(u => !isNonCombatUnit(u))

    trainWorkers(self).unsafePerformIO()
    buildSupplyStructures(game, self).unsafePerformIO()
    orders(self, game)

  }
}
