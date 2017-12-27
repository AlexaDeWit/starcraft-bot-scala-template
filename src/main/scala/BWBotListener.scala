import bwapi.{Unit => ScUnit, _}
import bwta.BWTA

import scalaz._
import Scalaz._
import BehaviourFunctions._

import scala.collection.JavaConverters._
import scalaz.effect.IO

object BWBotListener {
  def main(args: Array[String]): Unit =
    new BWBotListener().run()
}

class BWBotListener extends DefaultBWListener {
  val TILE_SIZE = 32
  val mirror = new Mirror()
  var game: Game = _
  var self: Player = _
  var builders: List[ScUnit] = List()

  def run(): Unit = {
    mirror.getModule.setEventListener(this)
    mirror.startGame()
  }

  def drawTerrainData(game: Game) = {
    val baseLocations = BWTA.getBaseLocations.asScala
    baseLocations.foreach(base => {
      val position = base.getTilePosition
      val leftTop = new Position(position.getX * TILE_SIZE, position.getY * TILE_SIZE)
      val rightBottom = new Position(leftTop.getX + 4 * TILE_SIZE, leftTop.getY + 3 * TILE_SIZE)
      game.drawBoxMap(leftTop, rightBottom, Color.Blue)

      base.getStaticMinerals.asScala.foreach( mineral =>
        game.drawCircleMap(mineral.getInitialPosition, 30, Color.Cyan)
      )

      base.getGeysers.asScala.foreach(geyser => {
        val p = geyser.getInitialTilePosition
        val tl = new Position(p.getX * TILE_SIZE, p.getY * TILE_SIZE)
        val br = new Position(tl.getX + 4 * TILE_SIZE, tl.getY + 2 * TILE_SIZE)
        game.drawBoxMap(tl, br, Color.Orange)
      })

      if(base.isIsland) game.drawCircleMap(base.getPosition, 80, Color.Yellow)
    })
    BWTA.getRegions.asScala.foreach(region => {
      val points = region.getPolygon.getPoints.asScala
      val items = points zip points.drop(1)
      for( (f, s) <- items ){
        game.drawLineMap(f, s, Color.Green)
      }

      region.getChokepoints.asScala.foreach(choke => {
        val p1 = choke.getSides.first
        val p2 = choke.getSides.second
        game.drawLineMap(p1, p2, Color.Red)
      })
    })
  }

  override def onUnitCreate(unit: ScUnit): Unit = {
    System.out.println("New unit " + unit.getType)
  }

  def assignGasGathering: Unit = {
    val currentlyGasMining = self.getUnits.asScala.filter(_.isGatheringGas)
    val refineries = self.getUnits.asScala.filter(_.getType.isRefinery)
    val neededGasGatherers = refineries.size * 3 -  currentlyGasMining.size
    if(neededGasGatherers > 0) {
      val minersToAdd = self.getUnits.asScala
        .filter(u => u.isGatheringMinerals || u.isIdle)
        .filterNot(u => builders.contains(u))
        .take(neededGasGatherers)
      val minersToGetGas = currentlyGasMining ++ minersToAdd
      val bundled = refineries.toList zip minersToGetGas.grouped(3).toList
      bundled.foreach( minerRefSet => {
        minerRefSet._2.foreach(unit => unit.gather(minerRefSet._1))
      })
    }
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

    //Kleislis
    withPackedGameState(List(
      buildSupplyStructures,
      trainWorkers,
      workerMine
    ))
  }

  def withPackedGameState(actions: List[Kleisli[IO, GameState, GameState]]): Unit = {
    val initial = IO(GameState(self, game, builders))
    val result = actions.foldRight(initial)((kl, gs) => kl =<< gs )
    unpackGameState(result)
  }

  def unpackGameState(state: IO[GameState]): Unit = {
    val ran = state.unsafePerformIO()
    builders = ran.designatedBuilders
  }


}
