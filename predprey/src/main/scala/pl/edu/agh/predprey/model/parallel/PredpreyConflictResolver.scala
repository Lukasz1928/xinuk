package pl.edu.agh.predprey.model.parallel

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model.{LettuceCell}
import pl.edu.agh.predprey.simulation.PredpreyMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object PredpreyConflictResolver extends ConflictResolver[PredpreyConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: PredpreyConfig): (GridPart, PredpreyMetrics) = {
    (current, incoming) match {
      case (Obstacle(), _) =>
        (Obstacle(), PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (LettuceCell(currentSmell, currentLifespan), EmptyCell(incomingSmell)) =>
        (LettuceCell(currentSmell + incomingSmell, currentLifespan), PredpreyMetrics.empty())

        //TODO

//      case (EmptyCell(currentSmell), PredpreyCell(incomingSmell)) =>
//        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (PredpreyCell(currentSmell), PredpreyCell(incomingSmell)) =>
//        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (PredpreyCell(currentSmell), LoudCell(incomingSmell)) =>
//        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (LoudCell(incomingSmell), PredpreyCell(currentSmell)) =>
//        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (LoudCell(currentSmell), EmptyCell(incomingSmell)) =>
//        (LoudCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (EmptyCell(incomingSmell), LoudCell(currentSmell)) =>
//        (LoudCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
//      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
