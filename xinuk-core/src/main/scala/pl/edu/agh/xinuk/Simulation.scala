package pl.edu.agh.xinuk

import java.awt.Color
import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.gui.GuiActor
import pl.edu.agh.xinuk.model.Grid.CellArray
import pl.edu.agh.xinuk.model.parallel.{ConflictResolver, Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.collection.immutable.TreeSet
import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig : ValueReader](
  metricHeaders: Vector[String],
  conflictResolver: ConflictResolver[ConfigType],
  smellPropagationFunction: (CellArray, Int, Int) => Vector[Option[SignalArray]],
  emptyCellFactory: => SmellingCell)(
  movesControllerFactory: (TreeSet[(Int, Int)], ConfigType) => MovesController,
  cellToColor: PartialFunction[GridPart, Color] = PartialFunction.empty
)(rawConfig: Config, implicit val config: ConfigType) extends LazyLogging {



  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)
  private val workerRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = WorkerActor.Name,
      entityProps = WorkerActor.props[ConfigType](workerRegionRef, movesControllerFactory, conflictResolver, smellPropagationFunction, emptyCellFactory),
      settings = ClusterShardingSettings(system),
      extractShardId = WorkerActor.extractShardId,
      extractEntityId = WorkerActor.extractEntityId
    )

  def start(): Unit = {
    if (config.isSupervisor) {

      val workers: Vector[WorkerId] =
        (1 to math.pow(config.workersRoot, 2).toInt)
          .map(WorkerId)(collection.breakOut)

      workers.foreach { id =>
        if (config.guiType != GuiType.None) {
          system.actorOf(GuiActor.props(workerRegionRef, id, cellToColor))
        }
        val neighbours: Vector[Neighbour] = NeighbourPosition.values.flatMap { pos =>
          pos.neighbourId(id).map(_ => Neighbour(pos))
        }(collection.breakOut)
        workerRegionRef ! WorkerActor.NeighboursInitialized(id, neighbours)
      }
    }
  }

}