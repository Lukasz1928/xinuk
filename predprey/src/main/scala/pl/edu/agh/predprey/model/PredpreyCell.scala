package pl.edu.agh.predprey.model

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SignalArray, SmellingCell}

final case class PredpreyCell(smell: SmellArray) extends SmellingCell {
  override type Self = PredpreyCell

  override def withSmell(smell: SmellArray): PredpreyCell = copy(smell = smell)
}

object PredpreyCell {
  def create(initialSignal: SignalArray)(implicit config: PredpreyConfig): PredpreyCell = PredpreyCell(Array.fill(Cell.Size, Cell.Size)(SignalArray(config.predpreyCellInitialSignal)))
}