package pl.edu.agh.predprey.algorithm

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model.{LoudCell, PredpreyCell}
import pl.edu.agh.predprey.simulation.PredpreyMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class PredpreyMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: PredpreyConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, PredpreyMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(3 * config.gridSize / 4)(3 * config.gridSize / 4) = PredpreyCell.create(SignalArray(config.predpreyInitialSignal))
    grid.cells(config.gridSize / 4)(config.gridSize / 4) = LoudCell.create(SignalArray(config.predpreyInitialSignal))

    val metrics = PredpreyMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, PredpreyMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell = cell match {
        case PredpreyCell(_) => PredpreyCell.create(SignalArray(config.predpreyInitialSignal))
        case LoudCell(_) => LoudCell.create(SignalArray(config.predpreyInitialSignal))
        case _ => EmptyCell(cell.smell)
      }

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, PredpreyCell(_)) => true
      case (_, _, LoudCell(_)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({
      case (x, y, cell) => copyCells(x, y, cell)
    })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, PredpreyMetrics.empty())
  }
}