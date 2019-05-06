package pl.edu.agh.predprey.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class PredpreyCell(smell: SmellArray) extends SmellingCell {
  override type Self = PredpreyCell

  override def withSmell(smell: SmellArray): PredpreyCell = copy(smell = smell)
}

object PredpreyCell {
  def create(initialSignal: Signal): PredpreyCell = PredpreyCell(Cell.emptySignal)
}