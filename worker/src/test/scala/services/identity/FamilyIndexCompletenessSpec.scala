package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.FamilyFinalReads
import services.movies.ListingKey

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.{Random, Try}

/**
 * THE FAMILY INDEX IS COMPLETE ([[FamilyIndex]]), over real slices: the committed drift slices
 * (`test/resources/fixtures/drift-slices/`) and pinned identity slices (`identity-slices/`), each replayed from its own
 * answers alone.
 *
 *  1. An index stands against the corpus it was read off: every recorded read digests as recorded (the recorder and a
 *     re-read digest alike), and every family resolved alone took what the corpus resolve took (A3) -- resolved against
 *     the corpus's context, and replayed from its recorded reads ALONE ([[FamilyIndex.replayAlone]]: no corpus, no lookup
 *     beyond what it recorded; a read outside them fails).
 *  2. A family whose index STANDS against another corpus decides there as it decided: for corpora that differ by a
 *     population outside it — some of the slice's listings taken out (a removal) or brought in (an addition), one other
 *     family's, one listing, a quarter, a half — every family whose entry [[FamilyIndex.Entry.stale]] finds nothing
 *     moved for keeps its members together, alone, and every member's take. A failure names a read the index lacks.
 */
class FamilyIndexCompletenessSpec extends AnyFlatSpec with Matchers {
  import FamilyIndexCompletenessSpec._

  private lazy val checked: Map[Path, Try[Outcome]] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try Slices.zip(scripts.ParallelTasks.all(Slices)(dir => Try(check(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  "the slices" should "be there" in { Slices should not be empty }

  Slices.foreach { dir =>
    "A family index" should s"stand against its own corpus, and a standing family decide as it decided, in ${Root.relativize(dir)}" in {
      val outcome = checked(dir).get
      info(s"${outcome.families} families, ${outcome.comparisons} family comparisons over ${Perturbations} corpora each way: " +
        s"${outcome.standing} standing, ${outcome.stale} stale; reads per family p50 ${outcome.readsMedian}")
      withClue("A3, listings whose take resolved alone differs from the corpus resolve's: ")(outcome.apart shouldBe empty)
      withClue("reads that do not digest as recorded against their own corpus: ")(outcome.selfStale shouldBe empty)
      withClue("families whose replay from their recorded reads alone differs from the corpus resolve: ")(outcome.alone shouldBe empty)
      withClue("families whose index stands but whose decision moved (a read the index lacks): ")(outcome.violations shouldBe empty)
      info(s"final take: ${outcome.finalComparisons} comparisons, ${outcome.finalStanding} standing; ${outcome.finalReads}")
      withClue("final take, families settled alone taking otherwise than the corpus's stage: ")(outcome.finalApart shouldBe empty)
      withClue("final take, a family's stage reads digesting otherwise in the corpus's stage: ")(outcome.finalSelfStale shouldBe empty)
      withClue("final take, families whose reads stand but whose final take moved (a read the index lacks): ")(outcome.finalViolations shouldBe empty)
    }
  }
}

object FamilyIndexCompletenessSpec {
  val Root: Path         = Path.of("test", "resources", "fixtures")
  val Perturbations: Int = 6
  /** How many of the stage's own lookups are withheld, each its own corpus. */
  val Withheld: Int      = 2

  private def dirs(path: Path): Seq[Path] =
    if (!Files.isDirectory(path)) Nil else Files.list(path).iterator().asScala.filter(Files.isDirectory(_)).toSeq
  /** Every drift slice (`drift-slices/<cc>/<name>`) and every pinned identity slice (`identity-slices/<name>`). */
  lazy val Slices: Seq[Path] =
    (dirs(Root.resolve("drift-slices")).flatMap(dirs) ++ dirs(Root.resolve("identity-slices"))).sortBy(_.toString)

  final case class Outcome(families: Int, comparisons: Int, standing: Int, stale: Int, readsMedian: Int, apart: Seq[String],
                           selfStale: Seq[String], alone: Seq[String], violations: Seq[String], finalComparisons: Int = 0,
                           finalStanding: Int = 0, finalApart: Seq[String] = Nil, finalSelfStale: Seq[String] = Nil,
                           finalViolations: Seq[String] = Nil, finalReads: String = "")

  /** What a family took in a corpus: each member's take, and whether its members are one family there and alone. */
  private def decided(whole: Resolution, members: Set[ListingKey]): (Map[ListingKey, Option[String]], Boolean) = {
    val takes  = members.toSeq.map(key => key -> whole.decisionOf.get(key).flatMap(PopulationMonotonicity.takeOf)).toMap
    val ids    = members.flatMap(whole.familyOf.get)
    val joined = ids.sizeIs == 1 && whole.familyOf.count { case (_, id) => ids(id) } == members.size
    (takes, joined)
  }

  /** `underlying` with one read `withheld`: answered as not known yet, as a lookup no store has filed. */
  private final class Withholding(underlying: IdentityLookups, withheld: Dependency) extends FamilyIndex.ReadingLookups {
    protected def read[A](dependency: Dependency, ask: => Answer[A]): Answer[A] = if (dependency == withheld) Answer.Unknown else ask
    def hasDetail(listing: Listing): Boolean = underlying.hasDetail(listing)
    override def numbersFilmsByTmdbId: Boolean = underlying.numbersFilmsByTmdbId
    protected def detailOf(listing: Listing): Answer[Option[DetailFacts]] = underlying.detail(listing)
    protected def searchOf(query: CandidateQuery): Answer[Seq[Hit]] = underlying.candidates(query)
    protected def recordOf(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = underlying.film(tmdbId)
    protected def releaseOf(tmdbId: Int): Answer[Option[java.time.LocalDate]] = underlying.releaseDay(tmdbId)
    protected def castOf(tmdbId: Int): Answer[Option[Seq[String]]] = underlying.cast(tmdbId)
    protected def overviewOf(tmdbId: Int): Answer[Option[ProseWords]] = underlying.overview(tmdbId)
  }

  /** One corpus: its listings and lookups, its family index, its whole final take, and each family's final take settled
   *  ALONE (in entry order). */
  private final case class Corpus(listings: Seq[Listing], lookups: IdentityLookups, built: FamilyIndex.Built,
                                  whole: FamilyFinalReads.Settled, alone: Seq[FamilyFinalReads.Settled])

  def check(dir: Path): Outcome = {
    val run      = new scripts.IdentityDrift.SliceRun(dir)
    val reads    = run.readsAt(new tools.MutableClock(run.slice.recordedAt))
    val recorded = scripts.IdentityDrift.memo(reads)
    val inputs   = ResolveInputs.Unpinned
    val all      = run.listings
    def corpus(listings: Seq[Listing], lookups: IdentityLookups, values: Boolean = false, at: java.time.Instant = run.slice.recordedAt,
               reads: scripts.IdentitySlice.Reads = reads): Corpus = {
      val built = FamilyIndex.build(listings, lookups, reads.normalizer, inputs, values = values)
      def settled(members: Seq[Listing]) = FamilyFinalReads.settle(reads, members, built.whole, lookups, at)
      Corpus(listings, lookups, built, settled(listings), built.index.entries.map(e => settled(listings.filter(l => e.members.contains(l.key)))))
    }
    def nowOf(c: Corpus) = FamilyIndex.now(c.listings, c.lookups, reads.normalizer, inputs, c.built)
    val full     = corpus(all, recorded, values = true)
    // the family replayed ALONE from its recorded reads -- no corpus, no lookups beyond them -- takes what the corpus took
    val alone    = full.built.index.entries.zip(full.built.recorded).flatMap { case (entry, kept) =>
      Try(FamilyIndex.replayAlone(kept, reads.normalizer, inputs)).fold(e => Seq(s"${entry.name}: $e"),
        takes => entry.takes.collect { case (key, take) if takes.get(key).flatten != take =>
          s"${key.venue} | ${key.rawTitle}: corpus ${take.getOrElse("pending")}, alone ${takes.get(key).flatten.getOrElse("pending")}" })
    }
    val selfStale = full.built.index.stale(nowOf(full)).map { case (entry, why) => s"${entry.name}: ${why.take(5).mkString(", ")}" }
    // the final take: each family settled alone takes what the whole corpus's stage took, and reads what it reads
    def line(key: ListingKey, a: Option[String], b: Option[String]) = s"${key.venue} | ${key.rawTitle}: ${a.getOrElse("pending")} -> ${b.getOrElse("pending")}"
    val finalApart = full.alone.flatMap(f => f.takes.collect { case (key, take) if full.whole.takes.get(key).flatten != take =>
      line(key, full.whole.takes.get(key).flatten, take) })
    val finalSelfStale = full.built.index.entries.zip(full.alone).flatMap { case (entry, f) =>
      Option(FamilyFinalReads.moved(f, full.whole)).filter(_.nonEmpty).map(why => s"${entry.name}: ${why.take(5).mkString(", ")}") }
    // corpora a population apart -- one other family, one listing, a quarter, a half -- and corpora one read apart: a
    // lookup some family's stage read withheld (seeded per slice)
    val rng      = new Random(scala.util.hashing.MurmurHash3.stringHash(dir.getFileName.toString))
    val families = full.built.index.entries
    val dropped: Seq[Set[ListingKey]] = (0 until Perturbations).map {
      case 0 if families.sizeIs >= 2 => families(rng.nextInt(families.size)).members.keySet
      case 1 | 2                     => Set(all(rng.nextInt(all.size)).key)
      case k                         => rng.shuffle(all.map(_.key)).take(math.max(1, all.size / (if (k <= 4) 4 else 2))).toSet
    }.filter(s => s.nonEmpty && s.size < all.size).distinct
    // the stage's own lookups first (a cast, a release, a namesake's record the model never read), then any
    // (a record's own answer the model reads too, off the corpus's context: withheld, the model's entry moves first)
    val modelRecords = families.flatMap(_.context.keys).collect { case FamilyIndex.ContextRead.Record(film) => film }.toSet
    val stageOnly = full.alone.flatMap(_.lookups.keys).distinct.filterNot(d => families.exists(_.answers.contains(d))).filter {
      case Dependency.Lookup(_, Dependency.Aspect.Answer, Question.FilmRecord(film)) => !modelRecords(film)
      case _                                                                       => true
    }.sortBy(Dependency.encode)
    val anyRead   = families.flatMap(_.answers.keys).distinct.sortBy(Dependency.encode)
    val withheld  = (rng.shuffle(stageOnly).take(Withheld) ++ rng.shuffle(anyRead).take(1)).distinct
    var comparisons, standing, stale, finalComparisons, finalStanding = 0
    val violations, finalViolations = Seq.newBuilder[String]
    def compare(from: Corpus, to: Corpus, label: String): Unit = {
      val now = nowOf(to)
      val present = to.listings.map(_.key).toSet
      from.built.index.entries.zip(from.alone).filter(_._1.members.keySet.subsetOf(present)).foreach { case (entry, settled) =>
        comparisons += 1
        val why = entry.stale(now)
        if (why.nonEmpty) stale += 1
        else {
          standing += 1
          val (takes, joined) = decided(to.built.whole, entry.members.keySet)
          if (!joined) violations += s"[$label] ${entry.name}: its members no longer one family alone"
          entry.takes.foreach { case (key, take) => if (takes(key) != take) violations += s"[$label] ${line(key, take, takes(key))}" }
          // the final take: its model stands, and every read its stage made digests as recorded
          finalComparisons += 1
          if (FamilyFinalReads.moved(settled, to.whole).isEmpty) {
            finalStanding += 1
            settled.takes.foreach { case (key, take) =>
              val there = to.whole.takes.get(key).flatten
              if (there != take) finalViolations += s"[$label] ${line(key, take, there)}"
            }
          }
        }
      }
    }
    dropped.foreach { gone =>
      val rest    = corpus(all.filterNot(l => gone(l.key)), recorded)
      compare(full, rest, s"removing ${gone.size}")
      compare(rest, full, s"adding ${gone.size}")
    }
    // a year on: the same listings and answers read by a clock past the year's turn (what a `Year` receipt reads)
    locally {
      val later      = run.slice.recordedAt.plus(java.time.Duration.ofDays(366))
      val laterReads = run.readsAt(new tools.MutableClock(later))
      val yearOn     = corpus(all, scripts.IdentityDrift.memo(laterReads), at = later, reads = laterReads)
      compare(full, yearOn, "a year on")
      compare(yearOn, full, "a year back")
    }
    withheld.foreach { read =>
      val without = corpus(all, new Withholding(recorded, read))
      compare(full, without, s"withholding ${Dependency.encode(read)}")
      compare(without, full, s"filing ${Dependency.encode(read)}")
    }
    val sizes = families.map(e => e.context.size + e.answers.size).sorted
    Outcome(families.size, comparisons, standing, stale, sizes.lift(sizes.size / 2).getOrElse(0),
      full.built.apart.map(k => s"${k.venue} | ${k.rawTitle}"), selfStale, alone, violations.result().distinct, finalComparisons,
      finalStanding, finalApart, finalSelfStale, finalViolations.result().distinct,
      s"receipts ${full.alone.map(_.receipts.size).sum}, stage lookups ${full.alone.map(_.lookups.size).sum}, stage-only ${stageOnly.size}, " +
        s"withheld ${withheld.map(Dependency.encode).mkString(" ")}, final takes moved by the stage ${full.whole.takes.count { case (k, t) =>
          full.built.whole.decisionOf.get(k).flatMap(PopulationMonotonicity.takeOf) != t }}")
  }
}
