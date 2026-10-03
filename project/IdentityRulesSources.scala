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
 * Paths are relative to `src/main/` (`scala/...`, `resources/...`), sorted.
 */
object IdentityRulesSources {

  /** The resolver, which makes every stored family, and the store that encodes and decodes them. */
  val Roots: Set[String] = Set(
    "scala/services/identity/IncrementalResolver.scala",
    "scala/services/identity/IdentityModelStore.scala")

  def closure(roots: Set[String], depends: String => Set[String]): Set[String] = {
    var seen = roots; var frontier = roots
    while (frontier.nonEmpty) { val next = frontier.flatMap(depends) -- seen; seen ++= next; frontier = next }
    seen
  }

  /** The sources `roots` reach in `analysis`. */
  def of(analysis: xsbti.compile.CompileAnalysis, roots: Set[String]): Seq[String] = {
    val relations = analysis.asInstanceOf[sbt.internal.inc.Analysis].relations
    def rel(ref: VirtualFileRef): String = {
      val marker = "src/main/"
      val i = ref.id.indexOf(marker)
      if (i >= 0) ref.id.substring(i + marker.length) else ref.id
    }
    val sourceOf    = relations.classes.reverseMap.map { case (cls, srcs) => cls -> srcs.map(rel) }
    val known       = relations.allSources.map(rel).toSet
    val missing     = roots -- known
    require(missing.isEmpty, s"identity rules roots not compiled: ${missing.mkString(", ")}")
    val rootClasses = relations.classes.all.collect { case (src, cls) if roots(rel(src)) => cls }.toSet
    val classes     = closure(rootClasses, cls => relations.internalClassDep.forward(cls))
    (classes.flatMap(c => sourceOf.getOrElse(c, Set.empty[String])) ++ roots).toSeq.sorted
  }

  /** The resources under `main / resources` the resolver may read: every one, except those whose name only sources
   *  outside `closure` mention (fonts, certificates). A resource no source names is kept — its reader is unknown. */
  def resources(main: File, closure: Seq[String]): Seq[String] = {
    val base     = main / "resources"
    val files    = (base ** "*").get.filter(_.isFile).map(f => IO.relativize(base, f).get).sorted
    val inside   = closure.toSet
    val sources  = (main / "scala" ** "*.scala").get.map(f => "scala/" + IO.relativize(main / "scala", f).get -> IO.read(f))
    files.filter { name =>
      val namers = sources.collect { case (path, text) if text.contains(name) => path }
      namers.isEmpty || namers.exists(inside)
    }.map("resources/" + _)
  }
}
