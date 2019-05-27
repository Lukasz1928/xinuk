package pl.edu.agh.predprey.model.parallel

import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model.{LettuceCell, RabbitCell, WolfCell}
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
      case (LettuceCell(currentSmell, currentEnergy, currentLifespan, signalInd), EmptyCell(incomingSmell)) =>
        (LettuceCell(currentSmell + incomingSmell, currentEnergy, currentLifespan, signalInd), PredpreyMetrics.empty())
      case (LettuceCell(currentSmell, currentEnergy, currentLifespan, signalInd), LettuceCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (LettuceCell(currentSmell + incomingSmell, currentEnergy, currentLifespan, signalInd), PredpreyMetrics.empty())
      case (LettuceCell(currentSmell, currentEnergy, currentLifespan, signalInd), RabbitCell(incomingSmell, energy, incomingLifespan, incomingSignalInd)) =>
        (RabbitCell(currentSmell + incomingSmell, energy + config.lettuceEnergeticCapacity, math.max(currentLifespan, incomingLifespan), incomingSignalInd), PredpreyMetrics.empty())
      case (LettuceCell(currentSmell, currentEnergy, currentLifespan, signalInd), WolfCell(incomingSmell, energy, incomingLifespan, incomingSignalInd)) =>
        (WolfCell(incomingSmell, energy, incomingLifespan, incomingSignalInd), PredpreyMetrics.empty())
      case (RabbitCell(currentSmell, currentEnergy, currentLifespan, signalInd), RabbitCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (RabbitCell(currentSmell + incomingSmell, currentEnergy + incomingEnergy, math.max(currentLifespan, incomingLifespan), incomingSignalInd), PredpreyMetrics.empty())
      case (RabbitCell(currentSmell, currentEnergy, currentLifespan, signalInd), LettuceCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (RabbitCell(currentSmell + incomingSmell, currentEnergy + config.lettuceEnergeticCapacity, math.max(currentLifespan, incomingLifespan), signalInd), PredpreyMetrics.empty())
      case (RabbitCell(currentSmell, currentEnergy, currentLifespan, signalInd), WolfCell(incomingSmell, energy, incomingLifespan, incomingSignalInd)) =>
        (WolfCell(currentSmell + incomingSmell, energy + config.rabbitEnergeticCapacity, math.max(currentLifespan, incomingLifespan), incomingSignalInd), PredpreyMetrics.empty())
      case (WolfCell(currentSmell, currentEnergy, currentLifespan, signalInd), WolfCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (WolfCell(currentSmell + incomingSmell, currentEnergy + incomingEnergy, math.max(currentLifespan, incomingLifespan), incomingSignalInd), PredpreyMetrics.empty())
      case (WolfCell(currentSmell, currentEnergy, currentLifespan, signalInd), RabbitCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (WolfCell(currentSmell + incomingSmell, currentEnergy + incomingEnergy, math.max(currentLifespan, incomingLifespan), incomingSignalInd), PredpreyMetrics.empty())
      case (WolfCell(currentSmell, currentEnergy, currentLifespan, signalInd), LettuceCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (WolfCell(currentSmell + incomingSmell, currentEnergy, currentLifespan, signalInd), PredpreyMetrics.empty())
      case (WolfCell(currentSmell, currentEnergy, currentLifespan, signalInd), EmptyCell(incomingSmell)) =>
        (WolfCell(currentSmell + incomingSmell, currentEnergy, currentLifespan, signalInd), PredpreyMetrics.empty())
      case (RabbitCell(currentSmell, currentEnergy, currentLifespan, signalInd), EmptyCell(incomingSmell)) =>
        (RabbitCell(currentSmell + incomingSmell, currentEnergy, currentLifespan, signalInd), PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), RabbitCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (EmptyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), LettuceCell(incomingSmell, incomingEnergy, incomingLifespan, incomingSignalInd)) =>
        (EmptyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (EmptyCell(currentSmell), WolfCell(incomingSmell, energy, incomingLifespan, incomingSignalInd)) =>
        (EmptyCell(currentSmell + incomingSmell), PredpreyMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
