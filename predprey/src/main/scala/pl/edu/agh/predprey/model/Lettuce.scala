package pl.edu.agh.predprey.model

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class LettuceCell(smell: SmellArray, energy: Energy, lifespan: Long, signalInd: Int) extends SmellingCell {
  override type Self = LettuceCell

  override def withSmell(smell: SmellArray): LettuceCell = copy(smell = smell)
}

trait LettuceAccessible[+T <: GridPart] {
  def withLettuce(energy: Energy, lifespan: Long): T
}

object LettuceAccessible {

  def unapply(arg: EmptyCell)(implicit config: PredpreyConfig): LettuceAccessible[LettuceCell] =
    new LettuceAccessible[LettuceCell] {
      override def withLettuce(energy: Energy, lifespan: Long): LettuceCell = LettuceCell(arg.smellWith(SignalArray(config.lettuceInitialSignal)), energy, lifespan, config.lettuceSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: PredpreyConfig):  LettuceAccessible[BufferCell] =
    new LettuceAccessible[BufferCell] {
      override def withLettuce(energy: Energy, lifespan: Long): BufferCell = BufferCell(LettuceCell(arg.smellWith(SignalArray(config.lettuceInitialSignal)), energy, lifespan, config.lettuceSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: PredpreyConfig): Option[LettuceAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}