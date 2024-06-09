package net.jtownson.xdfbinext.a2l

trait CharacteristicSummary {
  def name: String
  def description: String
  def referencedBy: Set[String]
}

object CharacteristicSummary {
  case class ValueSummary(name: String, description: String, referencedBy: Set[String], fnUnits: String)
      extends CharacteristicSummary

  case class ValBlkSummary(name: String, description: String, referencedBy: Set[String], fnUnits: String)
      extends CharacteristicSummary

  case class CurveSummary(
      name: String,
      description: String,
      referencedBy: Set[String],
      axisUnits: String,
      fnUnits: String
  ) extends CharacteristicSummary

  case class MapSummary(
      name: String,
      description: String,
      referencedBy: Set[String],
      xAxisUnits: String,
      yAxisUnits: String,
      fnUnits: String
  ) extends CharacteristicSummary
}
