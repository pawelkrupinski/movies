package kinowo.build

/**
 * THE SAME-FILE EDGES OF THE TEST-IMPACT SELECTION (project/TestImpact.scala): the class dependencies inside ONE source
 * file, which Zinc does not record (its compiler bridge drops a dependency on the source being compiled), read from the
 * file's class files the way the coupling report reads them (`Coupling.sameFileEdges`, project/ClassReferences.scala).
 * Without them a test class reached only through a class of its own file is never selected: on 2026-11-22
 * PopulationMonotonicityPart1Spec..Part4Spec, `extends PopulationMonotonicitySpec(n)` in that spec's file, whose object
 * (the one class Zinc charges the file's imports to) reached the changed `TitleRelation`, were left out, and Main went
 * red on Part2 (#2806).
 *
 * The edges are between Zinc's class names (source names: `a.Outer`, `a.Outer.Inner`), as `dependent <- dependency`
 * pairs: each top-level class of the file depends on every top-level class of it that its class files use (a parent
 * included: a subclass on its base), and each class name on every other name of its own top-level class (a nested class
 * and its enclosing one share one class file graph, and Zinc records neither's use of the other).
 *
 * Compiled twice: into the build (Scala 2.12) and into common's tests (Scala 3, SameFileEdgesSpec) -- so it is written in
 * the syntax both accept.
 */
object SameFileEdges {

  /** The `(dependency, dependent)` pairs, by Zinc class name, that one source file adds to Zinc's record: `names`, its
   *  Zinc class names (source names); `binaries`, each one's binary names (`a.Outer`, `a.Outer$`); `compiled`, its class
   *  files, read only for a file of two or more top-level classes. */
  def of(names: Set[String], binaries: Map[String, Set[String]])(compiled: => Seq[ClassReferences.Compiled]): Set[(String, String)] = {
    val topOfNormalized = Coupling.topLevel(names.map(Coupling.normalize))
    val topOf: Map[String, String] = names.map(n => n -> topOfNormalized(Coupling.normalize(n))).toMap
    val namesOf: Map[String, Set[String]] = topOf.toSeq.groupBy(_._2).map { case (top, v) => top -> v.map(_._1).toSet }
    val within = namesOf.values.toSeq.flatMap(group => for (a <- group; b <- group if a != b) yield a -> b)
    val across =
      if (namesOf.size < 2) Nil
      else {
        val binaryTops = binaries.toSeq.flatMap { case (name, bins) => bins.map(_ -> topOf(name)) }.toMap
        Coupling.sameFileEdges(compiled, binaryTops).toSeq.flatMap { case (user, used) =>
          for (dependency <- namesOf(used); dependent <- namesOf(user)) yield dependency -> dependent
        }
      }
    (within ++ across).toSet
  }

  /** Every class `seeds` reaches along `next`, `seeds` included. */
  def closure(seeds: Set[String], next: String => Set[String]): Set[String] = {
    var reached = seeds; var frontier = seeds
    while (frontier.nonEmpty) { val more = frontier.flatMap(next) -- reached; reached ++= more; frontier = more }
    reached
  }
}
