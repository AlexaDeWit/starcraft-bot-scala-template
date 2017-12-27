import bwapi.{Unit => ScUnit, _}

import scalaz._
import Scalaz._
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Random
import scalaz.effect.IO


object BehaviourFunctions {

  def getWorkers(player: Player): Iterable[ScUnit] = player.getUnits.asScala.filter(_.getType.isWorker)

  def isNonCombatUnit(unit: ScUnit): Boolean = unit.getType.isWorker || unit.getType.isBuilding

  def trainWorkers: Kleisli[IO, GameState, GameState] = {
    Kleisli( gameState => {
      val player = gameState.player
      val maxWorkers = 150 * player.getUnits.asScala.count(_.getType == UnitType.Protoss_Nexus)
      val workerCount = getWorkers(player).size
      val targetWorkers = maxWorkers - workerCount
      val potentialWorkers = player.minerals / 50
      player
        .getUnits
        .asScala
        .filter(u => u.getType == UnitType.Protoss_Nexus)
        .filter(u => u.isIdle)
        .take(Math.min(targetWorkers, potentialWorkers))
        .foldRight(0)((u: ScUnit, i: Int) => {
          u.train(UnitType.Protoss_Probe)
          i + 1
        })
      IO(gameState)
    })
  }

  def buildArmy(player: Player, game: Game): Unit = {
    if (player.getUnits.asScala.count(isArmyUnit) < 200) {
      trainUnit(player, UnitType.Protoss_Gateway, UnitType.Protoss_Zealot)
    }
  }

  def getMyWorkers(player: Player): List[ScUnit] = player.getUnits.asScala.filter(_.getType.isWorker).toList

  def designateABuilder: Option[ScUnit] = ???

  def buildStructure(game: Game, player: Player, buildingType: UnitType) = IO({
    val worker = designateABuilder
    val pair = worker.flatMap(w => {
      val tile = getBuildTile(game, player, buildingType, w, player.getStartLocation)
      if (tile.isDefined) System.out.println("Target tile found")
      tile.map(t => (t, w))
    })
    pair.foreach { case (tile: TilePosition, unit: ScUnit) =>
      unit.build(buildingType, tile)
    }
  })

  def trainUnit(player: Player, structure: UnitType, unit: UnitType) =
    player.getUnits.asScala.filter(_.getType == UnitType.Protoss_Gateway).filter(_.isIdle).foreach(s => s.train(unit))

  def isArmyUnit(unit: ScUnit) = !(unit.getType.isWorker || unit.getType.isBuilding || unit.getType.isNeutral || unit.getType.isAddon)


  def buildingPrioritizing(player: Player, game: Game, builders: List[ScUnit]): Unit = {
    val units = player.getUnits.asScala

    if (player.supplyTotal() - player.supplyUsed() <= 4) {
      buildStructure(game, player, UnitType.Protoss_Pylon)
    } else if (units.count(_.getType == UnitType.Protoss_Gateway) < 4) {
      buildStructure(game, player, UnitType.Protoss_Gateway)
    }
  }

  def workerMine: Kleisli[IO, GameState, GameState] = {
    Kleisli( gameState => {
      if(gameState.game.elapsedTime() < 10) {
        workerSplit.run(gameState)
      } else {
        val player = gameState.player
        val game = gameState.game
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
        IO(gameState)
      }
    })
  }

  def workerSplit: Kleisli[IO, GameState, GameState] = {
    Kleisli( gameState => {
      val player = gameState.player
      val game = gameState.game
      val workers = player.getUnits.asScala
        .filter(_.getType.isWorker)
        .filter(_.isIdle)
      val minerals = Random.shuffle(game.neutral.getUnits.asScala.filter(_.getType.isMineralField))
      (workers zip minerals).foreach{ case (worker, mineral) => worker.gather(mineral)}
      IO(gameState)
    })
  }

  def searchBuildableTile(game: Game, player: Player, buildingType: UnitType, builder: ScUnit, searchStartPoint: TilePosition, maxDist: Int, skip: Int): Option[TilePosition] = {
    val unskippedXS = Range(searchStartPoint.getX - maxDist, searchStartPoint.getX + maxDist)
    val unskippedYS = Range(searchStartPoint.getY - maxDist, searchStartPoint.getY + maxDist)
    val skippedXS = Range(searchStartPoint.getX - maxDist, searchStartPoint.getX - skip) ++ Range(searchStartPoint.getX + skip, searchStartPoint.getX + maxDist)
    val skippedYS = Range(searchStartPoint.getY - maxDist, searchStartPoint.getY - skip) ++ Range(searchStartPoint.getY + skip, searchStartPoint.getY + maxDist)
    val ys = for {
      x <- unskippedXS
      y <- skippedYS
    } yield (x,y)
    val xs = for {
      x <- unskippedYS
      y <- skippedXS
    } yield (x,y)
    val points = xs ++ ys
    val ps = Random.shuffle(points)
    ps.filter(p => game.canBuildHere(new TilePosition(p._1, p._2), buildingType, builder, false))
      .find(p => {
        !player.getUnits.asScala.toVector.any(u => {(u.getID != builder.getID) && (Math.abs(u.getTilePosition.getX - p._1) < 3) && (Math.abs(u.getTilePosition.getY - p._2) < 3)})
      })
      .map(p => new TilePosition(p._1, p._2))
  }

  def getBuildTile(game: Game, player: Player, buildingType: UnitType, builder: ScUnit, searchStartPoint: TilePosition): Option[TilePosition] = {
    @tailrec
    def loop(dist: Int, stopDist: Int, skip: Int): Option[TilePosition] = {
      searchBuildableTile(game, player, buildingType, builder, searchStartPoint, dist, skip) match {
        case Some(tile) => Some(tile)
        case None if dist < stopDist => loop(dist + 4, stopDist, dist)
        case None => None
      }
    }

    val maxDist = 6
    val stopDist = 40
    if (buildingType.isRefinery) {
      game
        .neutral()
        .getUnits
        .asScala
        .filter(_.getType == UnitType.Resource_Vespene_Geyser)
        .filter(u => Math.abs(u.getTilePosition.getX - searchStartPoint.getX) < stopDist)
        .find(u => Math.abs(u.getTilePosition.getY - searchStartPoint.getY) < stopDist)
        .map(_.getTilePosition)
    } else {
      loop(maxDist, stopDist, 0)
    }
  }

  def buildSupplyStructures: Kleisli[IO, GameState, GameState] = {
    Kleisli(gameState => {
      val player = gameState.player
      val game = gameState.game
      if (/*(player.supplyTotal() - player.supplyUsed() < 2) &&*/ player.minerals() >= 100) {
        val worker = player
          .getUnits
          .asScala
          .find(_.getType.isWorker)

        val pair = worker.flatMap(w => {
          val tile = getBuildTile(game, player, UnitType.Protoss_Pylon, w, player.getStartLocation)
          tile.map(t => (t, w))
        })
        pair.foreach { case (tile: TilePosition, unit: ScUnit) =>
          unit.build(UnitType.Protoss_Pylon, tile)
        }
      }
      IO(gameState)
    })
  }

}
