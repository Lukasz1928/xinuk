package pl.edu.agh.predprey.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model._
import pl.edu.agh.predprey.simulation.PredpreyMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet
import scala.util.Random

final class PredpreyMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: PredpreyConfig) extends MovesController {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, PredpreyMetrics) = {
    grid = Grid.empty(bufferZone, EmptyCell.Instance)
    var wolfCount = 0L
    var rabbitCount = 0L
    var lettuceCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.wolfSpawnChance) {
                wolfCount += 1
                WolfAccessible.unapply(EmptyCell.Instance).withWolf(config.wolfStartEnergy, 0)
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.rabbitSpawnChance) {
                rabbitCount += 1
                RabbitAccessible.unapply(EmptyCell.Instance).withRabbit(config.rabbitStartEnergy, 0)
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.lettuceSpawnChance) {
                lettuceCount += 1
                LettuceAccessible.unapply(EmptyCell.Instance).withLettuce(0)
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }
    val metrics = PredpreyMetrics.empty()
    (grid, metrics)
  }

  def calculatePossibleDestinations(cell: RabbitCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .map {
        case (signalArray, index) => (signalArray(config.lettuceSignalIndex), index)
      }
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
      }
  }

  def calculatePossibleDestinations(cell: WolfCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .map {
        case (signalArray, index) => (signalArray(config.rabbitSignalIndex), index)
      }
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.cells(i)(j))
      }
      .collectFirstOpt {
        case (i, j, currentCell@RabbitAccessible(_), RabbitAccessible(_)) =>
          (i, j, currentCell)
        case (i, j, currentCell@WolfAccessible(_), WolfAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, PredpreyMetrics) = {
    val newGrid = Grid.empty(bufferZone, EmptyCell.Instance)

    var wolfCount = 0L
    var rabbitCount = 0L
    var lettuceCount = 0L

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val emptyCells =
        Grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.cells(i)(j).opt
              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (emptyCells.nonEmpty) {
        val (newLettuceX, newLettuceY, newCell) = emptyCells(random.nextInt(emptyCells.size))
        newGrid.cells(newLettuceX)(newLettuceY) = newCell
      }
    }


    def makeMove(x: Int, y: Int): Unit = {
      grid.cells(x)(y) match {
        case Obstacle() =>
          newGrid.cells(x)(y) = Obstacle()
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: LettuceCell =>
          if (iteration % config.lettuceReproductionFrequency == 0) {
            reproduce(x, y) { case LettuceAccessible(accessible) => accessible.withLettuce(0) }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell.copy(lifespan = cell.lifespan + 1)
          }
        case cell: RabbitCell =>
          moveRabbit(cell, x, y)
        case cell: WolfCell =>
          moveWolf(cell, x, y)
      }
    }

    def moveRabbit(cell: RabbitCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, RabbitAccessible(destination))) =>
          newGrid.cells(i)(j) = destination.withRabbit(cell.energy, cell.lifespan)
          newGrid.cells(i)(j) match {
            case LettuceCell(_, _,_) =>
            case _ =>
          }
        case Opt((i, j, inaccessibleDestination)) =>
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.energy, cell.lifespan)
      }
    }

    def moveWolf(cell: WolfCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, WolfAccessible(destination))) =>
          newGrid.cells(i)(j) = destination.withWolf(cell.energy, cell.lifespan)
          newGrid.cells(i)(j) match {
            case RabbitCell(_, _, _,_) =>
            case _ =>
          }
        case Opt((i, j, inaccessibleDestination)) =>
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.energy, cell.lifespan)
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.cells(x)(y) match {
        case WolfCell(_, _, _,_) =>
          wolfCount += 1
        case RabbitCell(_,_,_,_) =>
          rabbitCount += 1
        case LettuceCell(_,_,_) =>
          lettuceCount += 1
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = PredpreyMetrics()
    (newGrid, metrics)
  }
}