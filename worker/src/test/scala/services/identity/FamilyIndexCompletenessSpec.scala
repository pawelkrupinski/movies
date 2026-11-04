package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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
    }
  }
}

object FamilyIndexCompletenessSpec {
  val Root: Path         = Path.of("test", "resources", "fixtures")
  val Perturbations: Int = 6

  private def dirs(path: Path): Seq[Path] =
    if (!Files.isDirectory(path)) Nil else Files.list(path).iterator().asScala.filter(Files.isDirectory(_)).toSeq
  /** Every drift slice (`drift-slices/<cc>/<name>`) and every pinned identity slice (`identity-slices/<name>`). */
  lazy val Slices: Seq[Path] =
    (dirs(Root.resolve("drift-slices")).flatMap(dirs) ++ dirs(Root.resolve("identity-slices"))).sortBy(_.toString)

  final case class Outcome(families: Int, comparisons: Int, standing: Int, stale: Int, readsMedian: Int, apart: Seq[String],
                           selfStale: Seq[String], alone: Seq[String], violations: Seq[String])

  /** What a family took in a corpus: each member's take, and whether its members are one family there and alone. */
  private def decided(whole: Resolution, members: Set[ListingKey]): (Map[ListingKey, Option[String]], Boolean) = {
    val takes  = members.toSeq.map(key => key -> whole.decisionOf.get(key).flatMap(PopulationMonotonicity.takeOf)).toMap
    val ids    = members.flatMap(whole.familyOf.get)
    val joined = ids.sizeIs == 1 && whole.familyOf.count { case (_, id) => ids(id) } == members.size
    (takes, joined)
  }

  def check(dir: Path): Outcome = {
    val run      = new scripts.IdentityDrift.SliceRun(dir)
    val reads    = run.readsAt(new tools.MutableClock(run.slice.recordedAt))
    val lookups  = scripts.IdentityDrift.memo(reads)
    val inputs   = ResolveInputs.Unpinned
    val all      = run.listings
    def built(listings: Seq[Listing], values: Boolean = false) = FamilyIndex.build(listings, lookups, reads.normalizer, inputs, values = values)
    def nowOf(listings: Seq[Listing], b: FamilyIndex.Built) = FamilyIndex.now(listings, lookups, reads.normalizer, inputs, b)
    val full     = built(all, values = true)
    // the family replayed ALONE from its recorded reads -- no corpus, no lookups beyond them -- takes what the corpus took
    val alone    = full.index.entries.zip(full.recorded).flatMap { case (entry, recorded) =>
      Try(FamilyIndex.replayAlone(recorded, reads.normalizer, inputs)).fold(e => Seq(s"${entry.name}: $e"),
        takes => entry.takes.collect { case (key, take) if takes.get(key).flatten != take =>
          s"${key.venue} | ${key.rawTitle}: corpus ${take.getOrElse("pending")}, alone ${takes.get(key).flatten.getOrElse("pending")}" })
    }
    val selfStale = full.index.stale(nowOf(all, full)).map { case (entry, why) => s"${entry.name}: ${why.take(5).mkString(", ")}" }
    // corpora differing by a population: one other family, one listing, a quarter, a half -- seeded per slice
    val rng      = new Random(scala.util.hashing.MurmurHash3.stringHash(dir.getFileName.toString))
    val families = full.index.entries.filter(_.members.sizeIs >= 1)
    val dropped: Seq[Set[ListingKey]] = (0 until Perturbations).map {
      case 0 if families.sizeIs >= 2 => families(rng.nextInt(families.size)).members.keySet
      case 1 | 2                     => Set(all(rng.nextInt(all.size)).key)
      case k                         => rng.shuffle(all.map(_.key)).take(math.max(1, all.size / (if (k <= 4) 4 else 2))).toSet
    }.filter(s => s.nonEmpty && s.size < all.size).distinct
    var comparisons, standing, stale = 0
    val violations = Seq.newBuilder[String]
    def compare(from: FamilyIndex.Built, to: FamilyIndex.Built, toListings: Seq[Listing], label: String): Unit = {
      val now = nowOf(toListings, to)
      val present = toListings.map(_.key).toSet
      from.index.entries.filter(_.members.keySet.subsetOf(present)).foreach { entry =>
        comparisons += 1
        val why = entry.stale(now)
        if (why.nonEmpty) stale += 1
        else {
          standing += 1
          val (takes, joined) = decided(to.whole, entry.members.keySet)
          if (!joined) violations += s"[$label] ${entry.name}: its members no longer one family alone"
          entry.takes.foreach { case (key, take) =>
            if (takes(key) != take) violations += s"[$label] ${key.venue} | ${key.rawTitle}: ${take.getOrElse("pending")} -> ${takes(key).getOrElse("pending")}"
          }
        }
      }
    }
    dropped.foreach { gone =>
      val rest    = all.filterNot(l => gone(l.key))
      val without = built(rest)
      compare(full, without, rest, s"removing ${gone.size}")
      compare(without, full, all, s"adding ${gone.size}")
    }
    val sizes = full.index.entries.map(e => e.context.size + e.answers.size).sorted
    Outcome(full.index.entries.size, comparisons, standing, stale, sizes.lift(sizes.size / 2).getOrElse(0),
      full.apart.map(k => s"${k.venue} | ${k.rawTitle}"), selfStale, alone, violations.result().distinct)
  }
}
