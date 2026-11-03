package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.Path
import java.time.{Clock, ZoneOffset}
import scala.util.Try

/**
 * THE ONE TITLE RELATION over REAL families (docs/investigations/2026-11-01-flip-unification.md, stage 2): in every
 * committed slice (the drift slices and the identity slices), every pair of listings the model's resolve joins by a
 * TITLE must-link (same title, same search form, original title, title segment) -- and every listing whose title is filed
 * in a group another listing's backing reads ([[IdentityMeasures.titleGroups]], the searches, the shapes) -- is a pair
 * retention keeps: one listing leaving while the other is billed stays in the model's input ([[ListingRetention.leave]]).
 * Retention reading a title key of its own let a listing the model joined leave while a present listing still needed it:
 * UK "The Ladykillers (1955)" beside "The Ladykillers | Cinematic Breakfast" (8dfe17752), DE "BLACK WATER (2025)" beside
 * "Black Water (GLOBALE)" (5959ba7bf) -- each went and came back with the order answers arrived in.
 */
abstract class TitleRelationSpec(part: Int) extends AnyFlatSpec with Matchers with tools.CountryScope {
  import TitleRelationSpec._

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("slices/")) Seq(PopulationMonotonicitySpec.countryOf(Path.of(testName.split(' ').last))) else Nil

  // this part's slices ([[SliceParts]]): the spec is TitleRelationPart<k>Spec, k = 1 to SliceParts.Count
  private val slices = SliceParts.of(part, PopulationMonotonicitySpec.sliceDirs)

  // the slices in the change's countries only: a test of another country cancels before it reads its slice's result
  private lazy val found: Map[Path, Try[Seq[String]]] = {
    val dirs = SliceParts.inScope(slices, countriesInScope)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try dirs.zip(scripts.ParallelTasks.all(dirs)(dir => Try(apart(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  if (SliceParts.holdsWholeChecks(part))
    "The pairs a slice relates by title" should "be found: not vacuous" in {
      val dir = PopulationMonotonicitySpec.sliceDirs.find(_.toString.contains("nerve-centre-londonderry-the-ladykillers")).get
      related(dir).linked should be > 0
    }

  if (SliceParts.holdsWholeChecks(part))
    "The pairs' retention, the forms read once" should "decide as ListingRetention.leave does, pair for pair" in {
      val dir             = PopulationMonotonicitySpec.sliceDirs.find(_.toString.contains("werkstattkino-camouflage")).get
      val (alike, pairs)  = keptAlike(dir)
      pairs should be > 0
      alike shouldBe pairs
    }

  slices.foreach { dir =>
    "A real family" should s"keep, when one leaves, every pair the model joins or backs by title: ${PopulationMonotonicitySpec.relative(dir)}" in {
      withClue("pairs related by title that retention drops: ")(found(dir).get shouldBe empty)
    }
  }
}

/** The parts of [[TitleRelationSpec]] ([[SliceParts]]): each its own class, so the identity shards spread them. */
class TitleRelationPart1Spec extends TitleRelationSpec(1)
class TitleRelationPart2Spec extends TitleRelationSpec(2)
class TitleRelationPart3Spec extends TitleRelationSpec(3)
class TitleRelationPart4Spec extends TitleRelationSpec(4)

object TitleRelationSpec {

  /** How many of a slice's pairs its resolve joins by a title must-link, and how many its backing reads. */
  final case class Related(linked: Int, backed: Int)

  private final case class Read(all: Seq[Listing], normalizer: services.movies.TitleNormalizer, resolution: Resolution)

  private def read(dir: Path): Read = {
    val slice     = IdentitySlice.readSlice(dir)
    val replaying = new tools.SliceHttp.Replaying(slice.http)
    val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed,
      tools.ProdState.none)
    val all       = (slice.core ++ slice.context).sorted.distinctBy(_.key)
    Read(all, reads.normalizer, IdentityResolver.resolve(all, scripts.IdentityDrift.memo(reads), reads.normalizer, ResolveInputs.Unpinned))
  }

  private val TitleReasons = Set(ConstraintEdges.SameTitle, ConstraintEdges.SameSearchForm, ConstraintEdges.OriginalTitle, ConstraintEdges.TitleSegment)

  /** The listing pairs the slice's resolve joins by title, and the pairs one backs the other's film by title group. */
  private def pairs(r: Read): (Seq[(Listing, Listing, String)], Seq[(Listing, Listing, String)]) = {
    val bySortKey = r.all.map(l => l.sortKey -> l).toMap
    val linked = r.resolution.edges.filter(edge => edge.must && TitleReasons(edge.reason)).flatMap(edge =>
      for { a <- bySortKey.get(edge.a); b <- bySortKey.get(edge.b) } yield (a, b, edge.reason))
    // the backing's own reading of the groups, not the relation's: a listing's title groups, searches and shapes
    def measured(l: Listing) = Evidence.of(l, None, TitleDecorations.resolver).measured
    val keyOf  = r.all.map(l => l -> IdentityMeasures.key(l.title)).toMap
    val byKey  = r.all.groupBy(keyOf)
    val backed = r.all.flatMap { a =>
      val m = measured(a)
      (IdentityMeasures.titleGroups(m) ++ IdentityMeasures.searchGroupsAny(m) ++ m.shapeKeys).distinct.flatMap(group =>
        byKey.getOrElse(group, Nil).filter(_.key != a.key).map(b => (b, a, s"backs by group '$group'")))
    }
    (linked, backed)
  }

  def related(dir: Path): Related = { val (l, b) = pairs(read(dir)); Related(l.size, b.size) }

  /** Whether retention keeps `gone` beside `present`: [[ListingRetention.leave]] of the one pair, each listing's title forms
   *  read ONCE per slice. The three-argument `leave` reads them again on every call -- three [[TitleRelation.of]] (the
   *  title shapes' regexes) per pair -- and a UK slice relates up to ~320k pairs: 88% of this spec's CPU, ~1,000 s of the
   *  identity shards' (profiled 2026-11-03). Same Showing, same forms: the very decision `leave(Seq(gone), Seq(present),
   *  Nil, normalizer)` takes, which "the forms read once" below holds pair for pair. */
  private def keeps(r: Read): (Listing, Listing) => Boolean = {
    val formsOf = r.all.map(l => l.key -> TitleRelation.of(l, r.normalizer)).toMap
    val forms   = (l: Listing) => formsOf(l.key)
    (gone, present) =>
      ListingRetention.leave(Seq(gone), Nil, new TitleRelation.Showing(Seq(forms(present)), Seq(forms(gone)), r.normalizer), forms).kept.nonEmpty
  }

  /** How many of the slice's related pairs `keeps` and the three-argument `leave` decide alike, of how many. */
  def keptAlike(dir: Path): (Int, Int) = {
    val r                = read(dir)
    val (linked, backed) = pairs(r)
    val kept             = keeps(r)
    val all              = (linked.flatMap { case (a, b, _) => Seq((a, b), (b, a)) } ++ backed.map { case (a, b, _) => (a, b) }).distinctBy {
      case (a, b) => (a.key, b.key)
    }
    (all.count { case (gone, present) => kept(gone, present) == ListingRetention.leave(Seq(gone), Seq(present), Nil, r.normalizer).kept.nonEmpty }, all.size)
  }

  /** Every related pair whose one listing retention drops while the other is billed. */
  def apart(dir: Path): Seq[String] = {
    val r = read(dir)
    val (linked, backed) = pairs(r)
    val kept = keeps(r)
    (linked.flatMap { case (a, b, why) => Seq((a, b, why), (b, a, why)) } ++ backed).distinctBy { case (a, b, _) => (a.key, b.key) }
      .collect { case (gone, present, why) if !kept(gone, present) =>
        s"${gone.venue} | ${gone.rawTitle} dropped beside ${present.venue} | ${present.rawTitle} ($why)" }
      .sorted
  }

  /** `TitleRelationSpec [slice dir...]`: each slice's related pairs that retention drops (every slice without arguments). */
  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) PopulationMonotonicitySpec.sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach { dir =>
      Try(apart(dir) -> related(dir)).fold(e => println(s"# ${PopulationMonotonicitySpec.relative(dir)}: FAILED $e"), { case (rows, rel) =>
        println(s"# ${PopulationMonotonicitySpec.relative(dir)}: ${rel.linked} linked, ${rel.backed} backed, ${rows.size} dropped")
        rows.take(10).foreach(r => println(s"    $r"))
      })
    }
  }
}
