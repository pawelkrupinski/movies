package services.identity.measures

/** One measurement: a category, a number, or missing (with which side is missing). */
sealed trait Measure
object Measure {
  final case class Category(value: String) extends Measure
  final case class Number(value: Double) extends Measure
  final case class Missing(side: String) extends Measure
}
