package pl.edu.agh.predprey.model

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class WolfCell(smell: SmellArray, energy: Energy, lifespan: Long) extends SmellingCell {
  override type Self = WolfCell

  override def withSmell(smell: SmellArray): WolfCell = copy(smell = smell)
}

trait WolfAccessible[+T <: GridPart] {
  def withWolf(energy: Energy, lifespan: Long): T
}

object WolfAccessible {

  def unapply(arg: RabbitCell)(implicit config: PredpreyConfig): WolfAccessible[WolfCell] =
    new WolfAccessible[WolfCell] {
      override def withWolf(energy: Energy, lifespan: Long): WolfCell = WolfCell(arg.smellWith(SignalArray(config.wolfInitialSignal)), energy + config.rabbitEnergeticCapacity, lifespan)
    }

  def unapply(arg: EmptyCell)(implicit config: PredpreyConfig): WolfAccessible[WolfCell] =
    new WolfAccessible[WolfCell] {
      override def withWolf(energy: Energy, lifespan: Long): WolfCell = WolfCell(arg.smellWith(SignalArray(config.wolfInitialSignal)), energy, lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: PredpreyConfig): WolfAccessible[BufferCell] =
    new WolfAccessible[BufferCell] {
      override def withWolf(energy: Energy, lifespan: Long): BufferCell = BufferCell(WolfCell(arg.smellWith(SignalArray(config.wolfInitialSignal)), energy, lifespan))
    }

  def unapply(arg: GridPart)(implicit config: PredpreyConfig): Option[WolfAccessible[GridPart]] = arg match {
    case cell: RabbitCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}