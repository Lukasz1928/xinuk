package pl.edu.agh.predprey.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SignalArray, SmellingCell}

final case class LoudCell(smell: SmellArray) extends SmellingCell {
  override type Self = LoudCell

  override def withSmell(smell: SmellArray): LoudCell = copy(smell = smell)
}


object LoudCell {
  def create(initialSignal: SignalArray): LoudCell = LoudCell(Array.fill(Cell.Size, Cell.Size)(SignalArray(Array(Signal(0), initialSignal(1)))))
}