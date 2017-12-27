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

  def trainWorkers(player: Player): IO[Int] = {
    IO({
      val maxWorkers = 15 * player.getUnits.asScala.count(_.getType == UnitType.Protoss_Nexus)
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
    })
  }

  def searchBuildableTile(game: Game, buildingType: UnitType, builder: ScUnit, searchStartPoint: TilePosition, maxDist: Int, skip: Int): Option[TilePosition] = {
    val xs = Range(searchStartPoint.getX - skip, searchStartPoint.getX - maxDist) ++ Range(searchStartPoint.getX + skip, searchStartPoint.getX + maxDist)
    val ys = Range(searchStartPoint.getY - skip, searchStartPoint.getY - maxDist) ++ Range(searchStartPoint.getY + skip, searchStartPoint.getY + maxDist)
    val nrps = for {
      x <- xs
      y <- ys
    } yield (x, y)
    val ps = Random.shuffle(nrps).toList
    println(ps)
    ps.filter(p => game.canBuildHere(new TilePosition(p._1, p._2), buildingType, builder, false))
      .find( p => {
        !game.getAllUnits.asScala.toList.any( (u: ScUnit) => {
          (u.getID != builder.getID) && (Math.abs(u.getTilePosition.getX - p._1) < 3) && (Math.abs(u.getTilePosition.getY - p._2) < 3)
        })
      })
      .map(p => new TilePosition(p._1, p._2))
  }

  def getBuildTile(game: Game, buildingType: UnitType, builder: ScUnit, searchStartPoint: TilePosition): Option[TilePosition] = {
    @tailrec
    def loop(dist: Int, stopDist: Int, skip: Int): Option[TilePosition] = {
      searchBuildableTile(game, buildingType, builder, searchStartPoint, dist, skip) match {
        case Some(tile) => Some(tile)
        case None if dist < stopDist => loop(dist + 2, stopDist, dist)
        case None => None
      }
    }
    val maxDist = 6
    val stopDist = 40
    if(buildingType.isRefinery) {
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

  def buildSupplyStructures(game: Game, player: Player): IO[Unit] = {
    IO({
      if((player.supplyTotal() - player.supplyUsed() < 2) && player.minerals() >= 100) {
        val worker = player
          .getUnits
          .asScala
          .find(_.getType.isWorker)

        val pair = worker.flatMap(w => {
          val tile = getBuildTile(game, UnitType.Protoss_Pylon, w, player.getStartLocation)
          if(tile.isDefined) System.out.println("Target tile found")
          tile.map(t => (t, w))
        })
        pair.foreach{ case (tile: TilePosition, unit: ScUnit) =>
          unit.build(UnitType.Protoss_Pylon, tile)
        }
      }
    })
  }

}
