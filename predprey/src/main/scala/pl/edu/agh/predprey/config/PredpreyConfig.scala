package pl.edu.agh.predprey.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, SignalArray}

final case class PredpreyConfig(
                             gridSize: Int,
                             guiCellSize: Int,
                             smellsNumber: Int,
                             signalSuppressionFactor: List[Double],
                             signalAttenuationFactor: List[Double],
                             workersRoot: Int,
                             shardingMod: Int,

                             guiType: GuiType,
                             isSupervisor: Boolean,
                             signalSpeedRatio: List[Int],
                             iterationsNumber: Long,

                             predpreyCellInitialSignal: Array[Signal],
                             loudCellInitialSignal: Array[Signal]
                           ) extends XinukConfig