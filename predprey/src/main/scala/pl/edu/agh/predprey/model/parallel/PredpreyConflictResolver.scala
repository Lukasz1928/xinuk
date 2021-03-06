package pl.edu.agh.predprey.model.parallel

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model.PredpreyCell
import pl.edu.agh.predprey.simulation.PredpreyMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object PredpreyConflictResolver extends ConflictResolver[PredpreyConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: PredpreyConfig): (GridPart, PredpreyMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) =>
        (Obstacle, PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (PredpreyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), PredpreyCell(incomingSmell)) =>
        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (PredpreyCell(currentSmell), PredpreyCell(incomingSmell)) =>
        (PredpreyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
