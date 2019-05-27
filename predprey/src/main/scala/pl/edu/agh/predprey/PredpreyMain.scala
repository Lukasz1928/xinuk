package pl.edu.agh.predprey

import java.awt.Color
import java.io.File

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.predprey.algorithm.PredpreyMovesController
import pl.edu.agh.predprey.config.PredpreyConfig
import pl.edu.agh.predprey.model._
import pl.edu.agh.predprey.model.parallel.PredpreyConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model._
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
      metricHeaders,
      PredpreyConflictResolver,
      DefaultSmellPropagation().calculateSmellAddendsStandard,
      EmptyCell.Instance)(new PredpreyMovesController(_)(_),
      {
        case LettuceCell(_, _,_) => Color.GREEN
        case RabbitCell(_, _, _, _) => Color.RED
        case WolfCell(_, _, _, _) => Color.GRAY
        case Obstacle() => Color.BLUE
        case _ => Color.WHITE
      })(rawConfig, config).start()
  }
}
