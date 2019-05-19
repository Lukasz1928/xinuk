package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.CellArray

final case class Grid(cells: CellArray) extends AnyVal {

  import Grid._

  def propagatedSignal(calculateSmellAddends: (CellArray, Int, Int) => Vector[Option[SignalArray]], x: Int, y: Int)(implicit config: XinukConfig): GridPart = {
    val current = cells(x)(y)
    current match {
      case Obstacle() => current
      case smelling: SmellMedium =>
        val currentSmell = current.smell
        val addends = calculateSmellAddends(cells, x, y)
        val (newSmell, _) = addends.foldLeft(Array.ofDim[SignalArray](Cell.Size, Cell.Size), 0) {
          case ((cell, index), signalOpt) =>
            val (i, j) = SubcellCoordinates(index)
              cell(i)(j) = (currentSmell(i)(j) * config.signalAttenuationFactor) + (signalOpt.getOrElse(SignalArray.Zero) * config.signalSuppressionFactor)
            (cell, index + 1)
        }
        newSmell(1)(1) = SignalArray.Zero
        smelling.withSmell(newSmell)
    }
  }
}

object Grid {

  type CellArray = Array[Array[GridPart]]

  def empty(bufferZone: Set[(Int, Int)], emptyCellFactory: => SmellingCell)(implicit config: XinukConfig): Grid = {
    val n = config.gridSize

    val values = Array.tabulate[GridPart](n, n) {
      case (x, y) if bufferZone.contains((x, y)) => BufferCell(emptyCellFactory)
      case (x, y) if x == 0 || x == n - 1 || y == 0 || y == n - 1 => Obstacle()
      case _ => emptyCellFactory
    }
    Grid(values)
  }

  val SubcellCoordinates: Vector[(Int, Int)] = {
    val pos = Vector(0, 1, 2)
    pos.flatMap(i => pos.collect { case j if !(i == 1 && j == 1) => (i, j) })
  }

  def neighbourCellCoordinates(x: Int, y: Int): Vector[(Int, Int)] = {
    val pos = Vector(-1, 0, 1)
    pos.flatMap(i => pos.collect {
      case j if !(i == 0 && j == 0) => (x + i, y + j)
    })
  }
}


final case class SignalArray(value: Array[Signal]) extends AnyVal {
  def apply(index: Int): Signal = value(index)

  def +(other: SignalArray) = SignalArray(value.zipWithIndex.map{
    case (signal, index) => signal + other(index)
  })

  def -(other: SignalArray) = SignalArray(value.zipWithIndex.map{
    case (signal, index) => signal - other(index)
  })

  def *(scalars: List[Double]) = SignalArray(value.zip(scalars).map(x => x._1 * x._2))

  def /(scalars: List[Double]) = SignalArray(value.zip(scalars).map(x => x._1 / x._2))
}

object SignalArray {
  implicit class SignalArrayOps(signalList: List[Signal]) {
    def toSignalVector: SignalArray = SignalArray(signalList.toArray)
  }

  final val Signals = 2
  def Zero(implicit config: XinukConfig) = SignalArray(Array.fill(config.smellsNumber)(Signal.Zero))
}

final case class Signal(value: Double) extends AnyVal with Ordered[Signal] {
  def +(other: Signal) = Signal(value + other.value)

  def -(other: Signal) = Signal(value - other.value)

  def *(factor: Double) = Signal(value * factor)

  def /(divisor: Double) = Signal(value / divisor)

  override def compare(that: Signal): Int = Ordering.Double.compare(value, that.value)
}

object Signal {
  final val Zero = Signal(0d)
}

final case class Energy(value: Double) extends AnyVal with Ordered[Energy] {
  override def compare(that: Energy): Int = Ordering.Double.compare(value, that.value)

  def -(other: Energy): Energy = Energy(value - other.value)

  def +(other: Energy): Energy = Energy(value + other.value)

  def *(factor: Double): Energy = Energy(value * factor)

  def unary_- : Energy = Energy(-value)
}

object Energy {
  final val Zero = Energy(0)
}

trait GridPart {
  def smell: SmellArray
}

trait SmellMedium extends GridPart {
  type Self <: SmellMedium

  import Cell._

  final def smellWith(added: SignalArray): SmellArray = {
    smell + added
  }

  final def smellWithout(deducted: SignalArray): SmellArray = {
    Array.tabulate(Cell.Size, Cell.Size)((i, j) => smell(i)(j) - deducted)
  }

  final def smellWithoutArray(deducted: SmellArray): SmellArray = {
    for (k <- 0 to deducted.length) {
      Array.tabulate(Cell.Size, Cell.Size)((i, j) => smell(k)(i)(j) - deducted(k)(i)(j))
    }
    smell
  }

  def withSmell(smell: SmellArray): Self
}

trait SmellingCell extends SmellMedium {
  override type Self <: SmellingCell
}

object Cell {

  type SmellArray = Array[Array[SignalArray]]

  implicit class SmellArrayOps(private val arr: SmellArray) extends AnyVal {
    def +(other: SmellArray): SmellArray = {
      Array.tabulate(Cell.Size, Cell.Size)((x, y) => arr(x)(y) + other(x)(y))
    }

    def +(added: SignalArray): SmellArray = {
      Array.tabulate(Cell.Size, Cell.Size)((i, j) => arr(i)(j) + added)
    }
  }

  final val Size: Int = 3

  def emptySignal(implicit config: XinukConfig): SmellArray = Array.fill(Cell.Size, Cell.Size)(SignalArray.Zero)


}

final case class Obstacle()(implicit config: XinukConfig) extends GridPart {
  override val smell: SmellArray = Array.fill(Cell.Size, Cell.Size)(SignalArray.Zero)
}

final case class BufferCell(cell: SmellingCell) extends SmellMedium with GridPart {

  override type Self = BufferCell

  override def smell: SmellArray = cell.smell

  override def withSmell(smell: SmellArray): BufferCell = copy(cell.withSmell(smell))
}

final case class EmptyCell(smell: SmellArray)(implicit config: XinukConfig) extends SmellingCell {
  override type Self = EmptyCell

  override def withSmell(smell: SmellArray): EmptyCell = copy(smell)
}

object EmptyCell {
  final def Instance(implicit config: XinukConfig): EmptyCell = EmptyCell(Cell.emptySignal)
}