package pl.edu.agh.predprey.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Energy, Signal, SignalArray}

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

                             spawnChance: Double,
                             wolfSpawnChance: Double,
                             rabbitSpawnChance: Double,
                             lettuceSpawnChance: Double,
                             wolfStartEnergy: Energy,
                             rabbitStartEnergy: Energy,

                             lettuceReproductionFrequency: Int,

                             lettuceEnergeticCapacity: Energy,
                             rabbitEnergeticCapacity: Energy,

                             lettuceInitialSignal: Array[Signal],
                             rabbitInitialSignal: Array[Signal],
                             wolfInitialSignal: Array[Signal]
                           ) extends XinukConfig