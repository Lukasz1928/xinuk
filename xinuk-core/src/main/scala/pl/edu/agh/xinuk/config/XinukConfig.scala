package pl.edu.agh.xinuk.config

import com.avsystem.commons.misc.{AbstractNamedEnumCompanion, NamedEnum}

trait XinukConfig {
  def gridSize: Int
  def guiCellSize: Int
  def smellsNumber: Int
  def signalSuppressionFactor: List[Double]
  def signalAttenuationFactor: List[Double]
  def workersRoot: Int
  def shardingMod: Int

  def guiType: GuiType
  def isSupervisor: Boolean
  def signalSpeedRatio: List[Int]
  def iterationsNumber: Long
}

sealed trait GuiType extends NamedEnum

object GuiType extends AbstractNamedEnumCompanion[GuiType] {

  case object None extends GuiType {
    override val name: String = "none"
  }

  case object Basic extends GuiType {
    override def name: String = "basic"
  }

  override val values: List[GuiType] = caseObjects
}
