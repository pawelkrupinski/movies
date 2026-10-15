import sbt._
import xsbti.VirtualFileRef

/**
 * What the identity model's rules version digests (`services.identity.IdentityRules`): the sources the resolver is
 * built from, found STRUCTURALLY — every source the compiler records `Roots` as reaching, class to class through
 * every dependency Zinc keeps (a name used, a type inferred, a parent, an implicit: whatever the typer resolved),
 * transitively — and the resources it may read. A source the closure does not reach cannot change a decision the
 * resolver makes from the same inputs; the inputs themselves (listings, TMDB answers, venue pages) are checked per
 * family by its slice digest, and the calibration, decorations and pins by `IncrementalResolver.rulesVersion`.
 *
 * The resolver is its own module (`identity`) built on `common`, so the walk crosses from one module's compile analysis
 * into the other's: a class's dependencies are those Zinc records inside its module and those on another module's
 * classes (its external dependencies), followed wherever one of `modules` compiled them.
 *
 * Paths are the repository's (`identity/src/main/scala/...`, `common/src/main/resources/...`), sorted.
 */
object IdentityRulesSources {

  /** The resolver, which makes every stored family, and the store that encodes and decodes them. */
  val Roots: Set[String] = Set(
    "identity/src/main/scala/services/identity/IncrementalResolver.scala",
    "identity/src/main/scala/services/identity/IdentityModelStore.scala")

  /** The agreement stage (`AgreementStage.RulesVersion`): its rules and all they reach — the resolver its families run,
   *  the agreement's guards — so a deploy that changes any of them decides every stored verdict again, and one that
   *  changes none keeps them across the restart. */
  val AgreementRoots: Set[String] = Set("identity/src/main/scala/services/identity/agreement/AgreementStage.scala")

  /** What builds the identity projection's venue slots (`VenueSlotMemo.codeVersion`): `VenueSlots.scala`, which holds
   *  the slot memo, its key and the venue build, and what it calls to build a slot — the slot builder and the landing's
   *  same-title fold, with all they reach. Not the projection plan: it held them until 2026-10-04, and every change to
   *  the rest of it rebuilt every slot of every country on the next boot (a US first tick 65 s, not ~25). */
  val VenueSlotRoots: Set[String] = Set(
    "identity/src/main/scala/services/identity/VenueSlots.scala",
    "common/src/main/scala/services/movies/CinemaSlotBuilder.scala",
    "common/src/main/scala/services/movies/ScrapeListing.scala",
    "common/src/main/scala/services/movies/MixedFilmDetector.scala",
    "common/src/main/scala/services/movies/MovieRecordMerge.scala",
    "common/src/main/scala/services/movies/ShowtimesDigest.scala",
    "common/src/main/scala/services/movies/ListingKey.scala",
    "common/src/main/scala/services/movies/ScreeningTokens.scala",
    "common/src/main/scala/services/movies/VenuePageFacts.scala")

  /** Of what the venue slot code reaches, the sources digested as text but not followed, nor the resources only they
   *  name read: each reaches the whole
   *  resolver (its decisions, calibration and learned decorations, the stores) beside the little a slot is built
   *  by. Followed, every deploy that touched any of the resolver moved the venue slot version, and no restarted
   *  worker reused a stored slot (2026-10-04). `VenueSlots` builds a slot by the roots above; the landing's
   *  fold asks `ListingConstraints` only `venueCreditsApart`, which reads `MixedFilmDetector`, a root. */
  val VenueSlotLeaves: Set[String] = Set(
    "identity/src/main/scala/services/identity/VenueSlots.scala",
    "common/src/main/scala/services/movies/ListingConstraints.scala")

  def closure(roots: Set[String], depends: String => Set[String]): Set[String] = {
    var seen = roots; var frontier = roots
    while (frontier.nonEmpty) { val next = frontier.flatMap(depends) -- seen; seen ++= next; frontier = next }
    seen
  }

  /** One module's compile analysis, under its directory's name. */
  final case class Module(name: String, analysis: xsbti.compile.CompileAnalysis)

  /** The sources `roots` reach across `modules`, not following what a source of `leaves` reaches. */
  def of(modules: Seq[Module], roots: Set[String], leaves: Set[String] = Set.empty): Seq[String] = {
    val all = modules.map(m => m -> m.analysis.asInstanceOf[sbt.internal.inc.Analysis].relations)
    def rel(module: Module, ref: VirtualFileRef): String = {
      val marker = "src/main/"
      val i = ref.id.indexOf(marker)
      if (i >= 0) s"${module.name}/$marker${ref.id.substring(i + marker.length)}" else ref.id
    }
    val sourceOf: Map[String, Set[String]] = all.flatMap { case (m, r) =>
      r.classes.reverseMap.map { case (cls, srcs) => cls -> srcs.map(rel(m, _)) } }.toMap
    val known       = all.flatMap { case (m, r) => r.allSources.map(rel(m, _)) }.toSet
    val missing     = roots -- known
    require(missing.isEmpty, s"versioned roots not compiled: ${missing.mkString(", ")}")
    val rootClasses = all.flatMap { case (m, r) => r.classes.all.collect { case (src, cls) if roots(rel(m, src)) => cls } }.toSet
    // Another module's classes are recorded by their binary names ("services.movies.ScrapeHealth$",
    // "services.movies.ListingConstraints$CannotLink", a file's top-level definitions "services.movies.Foo$package"), its
    // own by their source names: the module that compiled the class maps one to the other.
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, r) =>
      r.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    def depends(cls: String): Set[String] =
      all.flatMap { case (_, r) =>
        r.internalClassDep.forward(cls) ++ r.externalClassDep.forward(cls).map(c => sourceNameOf.getOrElse(c, c)).filter(sourceOf.contains)
      }.toSet
    val classes     = closure(rootClasses, cls =>
      if (sourceOf.getOrElse(cls, Set.empty[String]).exists(leaves)) Set.empty else depends(cls))
    (classes.flatMap(c => sourceOf.getOrElse(c, Set.empty[String])) ++ roots).toSeq.sorted
  }

  /** The resources under each of `modules`' `src/main/resources` the resolver may read: every one, except those whose
   *  name only sources outside `closure` mention (fonts, certificates). A resource no source names is kept — its reader
   *  is unknown. */
  def resources(repo: File, modules: Seq[String], closure: Seq[String]): Seq[String] = {
    val inside  = closure.toSet
    val sources = modules.flatMap { m =>
      val scala = repo / m / "src" / "main" / "scala"
      (scala ** "*.scala").get.map(f => s"$m/src/main/scala/" + IO.relativize(scala, f).get -> IO.read(f))
    }
    modules.flatMap { m =>
      val base = repo / m / "src" / "main" / "resources"
      (base ** "*").get.filter(_.isFile).map(f => IO.relativize(base, f).get).sorted.filter { name =>
        val namers = sources.collect { case (path, text) if text.contains(name) => path }
        namers.isEmpty || namers.exists(inside)
      }.map(name => s"$m/src/main/resources/$name")
    }.sorted
  }
}
