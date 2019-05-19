package pl.edu.agh.predprey

import java.awt.Color
import java.io.File

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.predprey.algorithm.PredpreyMovesController
import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model.{LoudCell, PredpreyCell}
import pl.edu.agh.predprey.model.parallel.PredpreyConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, EmptyCell, Obstacle, SmellingCell}
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.util.{Failure, Success, Try}

object PredpreyMain extends LazyLogging {
  private val configPrefix = "predprey"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("xinuk.conf")))
      .filter(_.hasPath(configPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))

    def logHeader: String = s"worker:${metricHeaders.mkString(";")}"

    implicit val config: PredpreyConfig = {
      val forminConfig = rawConfig.getConfig(configPrefix)
      logger.info(WorkerActor.MetricsMarker, forminConfig.root().render(ConfigRenderOptions.concise()))
      logger.info(WorkerActor.MetricsMarker, logHeader)

      import net.ceedubs.ficus.Ficus._
      Try(forminConfig.as[PredpreyConfig]("config")) match {
        case Success(parsedConfig) =>
          parsedConfig
        case Failure(parsingError) =>
          logger.error("Config parsing error.", parsingError)
          System.exit(2)
          throw new IllegalArgumentException
      }
    }

    new Simulation(
      configPrefix,
      metricHeaders,
      PredpreyConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard,
      EmptyCell.Instance)(new PredpreyMovesController(_)(_),
      {
        case PredpreyCell(_) => Color.WHITE
        case LoudCell(_) => Color.WHITE
        case Obstacle() => Color.BLUE
        case cell: SmellingCell => cellToColorRegions(cell)
      }).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue0 = cell.smell.map(_.map(_.value(0)).max).max.value.toFloat
    val smellValue1 = cell.smell.map(_.map(_.value(1)).max).max.value.toFloat
    if (smellValue0 >= smellValue1) {
      val smellValue = smellValue0
      val brightness = Math.pow(smellValue, 0.1).toFloat
      if (smellValue < 0.00001) {
        val hue = 1f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else if (smellValue < 0.001) {
        val hue = 1f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else if (smellValue < 0.1) {
        val hue = 1f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else {
        val hue = 1f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      }
    }
    else {
      val smellValue = smellValue1
      val brightness = Math.pow(smellValue, 0.1).toFloat
      if (smellValue < 0.00001) {
        val hue = 0.3f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else if (smellValue < 0.001) {
        val hue = 0.3f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else if (smellValue < 0.1) {
        val hue = 0.3f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      } else {
        val hue = 0.3f
        val saturation = 1f
        Color.getHSBColor(hue, saturation, brightness)
      }
    }
  }
}