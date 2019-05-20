package pl.edu.agh.predprey.model

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class LettuceCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = LettuceCell

  override def withSmell(smell: SmellArray): LettuceCell = copy(smell = smell)
}

trait LettuceAccessible[+T <: GridPart] {
  def withLettuce(lifespan: Long): T
}

object LettuceAccessible {

  def unapply(arg: EmptyCell)(implicit config: PredpreyConfig): LettuceAccessible[LettuceCell] =
    new LettuceAccessible[LettuceCell] {
      override def withLettuce(lifespan: Long): LettuceCell = LettuceCell(arg.smellWith(SignalArray(config.lettuceInitialSignal)), lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: PredpreyConfig):  LettuceAccessible[BufferCell] =
    new LettuceAccessible[BufferCell] {
      override def withLettuce(lifespan: Long): BufferCell = BufferCell(LettuceCell(arg.smellWith(SignalArray(config.lettuceInitialSignal)), lifespan))
    }

  def unapply(arg: GridPart)(implicit config: PredpreyConfig): Option[LettuceAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}