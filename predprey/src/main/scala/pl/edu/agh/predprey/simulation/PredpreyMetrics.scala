package pl.edu.agh.predprey.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class PredpreyMetrics() extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): PredpreyMetrics = {
    this
  }
}

object PredpreyMetrics {
  private val EMPTY = PredpreyMetrics()

  def empty(): PredpreyMetrics = EMPTY
}