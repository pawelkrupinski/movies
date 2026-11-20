import sbt._

/**
 * THE SPECS THAT REACH A TOUCHED METHOD (`touchedSpecs`, scripts/mutation-check and scripts/coverage-touched), read from
 * Zinc's record -- narrower than the test-impact selection (project/TestImpact.scala), which follows every dependency
 * transitively and so, for a helper every scraper shares, names a module's whole suite (230 scraper specs for one
 * ScraperParse method, 2026-11-20). Two steps out from the method's source, by name:
 *
 *  - every spec depending on a class of the method's source (its own spec, and any spec using the class);
 *  - every spec depending on a production class that both depends on the method's source AND uses the method's name
 *    (the caller's spec: HeliosRestSpec for ScraperParse.jsonObjectNames, which HeliosRest calls).
 *
 * A spec is a test class sbt discovered (`definedTestNames`), nested classes folded into their top-level class. What
 * this misses -- a spec reaching the method three calls away -- is what a mutant it would have killed survives for,
 * and the report says so: such a method wants a closer pinning test anyway.
 */
object TouchedSpecs {

  /** `touched`: (repository path of a source, method name); `tests`: module -> its discovered test classes. Each
   *  reaching spec as `<module> <class>`, sorted. */
  def select(repo: File, units: Seq[TestImpact.Unit], tests: Map[String, Seq[String]], touched: Seq[(String, String)]): Seq[String] = {
    val all = units.map(u => u -> u.analysis.asInstanceOf[sbt.internal.inc.Analysis])
    val testOf: Map[String, String] = tests.toSeq.flatMap { case (m, names) => names.map(_ -> m) }.toMap
    val classesOf: Map[String, Set[String]] = all.flatMap { case (_, a) =>
      a.relations.classes.forwardMap.toSeq.map { case (src, cls) => TestImpact.repoPath(repo, src) -> cls.toSet }
    }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val sourceOfClass: Map[String, String] = classesOf.toSeq.flatMap { case (src, cls) => cls.map(_ -> src) }.toMap
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, a) =>
      a.relations.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    // dependency (a class) -> its dependents (classes)
    val dependents: Map[String, Set[String]] = all.flatMap { case (_, a) =>
      a.relations.internalClassDep.all.toSeq ++ a.relations.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }
    }.groupBy(_._2).map { case (k, v) => k -> v.map(_._1).toSet }
    val usedNames: Map[String, Set[String]] = all.flatMap { case (_, a) => sbt.internal.inc.UsedNamesAccess.of(a.relations).toSeq }
      .groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    def isMain(cls: String): Boolean = sourceOfClass.get(cls).exists(_.matches("[^/]+/src/main/.*"))
    def specOf(cls: String): Option[String] = {
      val top = Iterator.iterate(cls)(c => c.substring(0, math.max(c.lastIndexOf('.'), 0))).takeWhile(_.nonEmpty).find(testOf.contains)
      top.map(t => s"${testOf(t)} $t")
    }
    touched.flatMap { case (source, method) =>
      val own      = classesOf.getOrElse(source, Set.empty)
      val direct   = own.flatMap(c => dependents.getOrElse(c, Set.empty)) -- own
      val callers  = direct.filter(c => isMain(c) && usedNames.getOrElse(c, Set.empty)(method))
      val callerDependents = callers.flatMap(c => dependents.getOrElse(c, Set.empty))
      (direct ++ callerDependents).toSeq.flatMap(specOf)
    }.distinct.sorted
  }
}
