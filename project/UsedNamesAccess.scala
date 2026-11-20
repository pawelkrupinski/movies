package sbt.internal.inc

/** The names each compiled class uses, as Zinc records them (`Relations.names`, which Zinc keeps `private[inc]`): what
 *  project/DeadCode.scala and project/TouchedSpecs.scala read a member's callers by. Class name -> the names it uses. */
object UsedNamesAccess {
  def of(relations: Relations): Map[String, Set[String]] =
    relations.names.toMultiMap.iterator.map { case (cls, used) => cls -> used.iterator.map(_.name).toSet }.toMap
}
