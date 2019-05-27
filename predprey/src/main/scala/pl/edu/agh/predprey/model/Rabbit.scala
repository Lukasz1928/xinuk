package pl.edu.agh.predprey.model

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class RabbitCell(smell: SmellArray, energy: Energy, lifespan: Long, signalInd: Int) extends SmellingCell {
  override type Self = RabbitCell

  override def withSmell(smell: SmellArray): RabbitCell = copy(smell = smell)
}

trait RabbitAccessible[+T <: GridPart] {
  def withRabbit(energy: Energy, lifespan: Long): T
}

object RabbitAccessible {

  def unapply(arg: LettuceCell)(implicit config: PredpreyConfig): RabbitAccessible[RabbitCell] =
    new RabbitAccessible[RabbitCell] {
      override def withRabbit(energy: Energy, lifespan: Long): RabbitCell = RabbitCell(arg.smellWith(SignalArray(config.rabbitInitialSignal)), energy + config.lettuceEnergeticCapacity, lifespan, config.rabbitSignalIndex)
    }

  def unapply(arg: EmptyCell)(implicit config: PredpreyConfig): RabbitAccessible[RabbitCell] =
    new RabbitAccessible[RabbitCell] {
      override def withRabbit(energy: Energy, lifespan: Long): RabbitCell = RabbitCell(arg.smellWith(SignalArray(config.rabbitInitialSignal)), energy, lifespan, config.rabbitSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: PredpreyConfig): RabbitAccessible[BufferCell] =
    new RabbitAccessible[BufferCell] {
      override def withRabbit(energy: Energy, lifespan: Long): BufferCell = BufferCell(RabbitCell(arg.smellWith(SignalArray(config.rabbitInitialSignal)), energy, lifespan, config.rabbitSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: PredpreyConfig): Option[RabbitAccessible[GridPart]] = arg match {
    case cell: LettuceCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}