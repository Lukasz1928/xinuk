package pl.edu.agh.predprey.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Energy, Signal}

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

                             lettuceStartEnergy: Energy,
                             rabbitStartEnergy: Energy,
                             wolfStartEnergy: Energy,

                             lettuceReproductionFrequency: Int,
                             rabbitReproductionFrequency: Int,
                             wolfReproductionFrequency: Int,

                             lettuceEnergeticCapacity: Energy,
                             rabbitEnergeticCapacity: Energy,

                             lettuceInitialSignal: Array[Signal],
                             lettuceSignalIndex: Int,
                             rabbitInitialSignal: Array[Signal],
                             rabbitSignalIndex: Int,
                             wolfInitialSignal: Array[Signal],
                             wolfSignalIndex: Int,

                             lettuceLifeActivityCost: Energy,
                             rabbitLifeActivityCost: Energy,
                             wolfLifeActivityCost: Energy
                           ) extends XinukConfig