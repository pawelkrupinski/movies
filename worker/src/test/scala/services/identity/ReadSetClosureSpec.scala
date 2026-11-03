package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.IdentitySlice

import java.nio.file.Path
import java.time.{Clock, ZoneOffset}
import scala.util.Try
import scala.util.hashing.MurmurHash3

/**
 * READ-SET CLOSURE over REAL families (docs/investigations/2026-11-01-flip-unification.md, stage 4): property 1 of the
 * 10-30 plan -- as answers are only FILED, a whole resolve refines each take: Pending, then one decided value, never
 * another -- held on every committed slice, its answers withheld and filed in [[Rounds]] rounds of a seeded order. A take
 * decided while a question of a listing it READ is open moves when that answer is filed: the decision awaits every open
 * question of every listing whose evidence it read -- its family's searches, pages and records, beyond its own cluster
 * and its must-links' reach (#595, Torrington: its reach only).
 */
abstract class ReadSetClosureSpec(part: Int) extends AnyFlatSpec with Matchers with tools.CountryScope {
  import ReadSetClosureSpec._

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("slices/")) Seq(PopulationMonotonicitySpec.countryOf(Path.of(testName.split(' ').last))) else Nil

  // this part's slices ([[SliceParts]]): the spec is ReadSetClosurePart<k>Spec, k = 1 to SliceParts.Count
  private val slices = SliceParts.of(part, PopulationMonotonicitySpec.sliceDirs)

  /** Each slice in the change's countries read ONCE, and both its checks' findings from that reading, the slices side by
   *  side (a test of another country cancels before it reads its slice's result). */
  private lazy val found: Map[Path, (Try[Seq[String]], Try[Seq[String]])] = {
    val dirs = SliceParts.inScope(slices, countriesInScope)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try dirs.zip(scripts.ParallelTasks.all(dirs) { dir =>
      val read = Try(Read(dir))
      (read.flatMap(r => Try(moved(r) ++ arrivals(r))), read.flatMap(r => Try(unreadSibling(r))))
    }(using scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  slices.foreach { dir =>
    "A real family" should s"decide no take a later filing moves: ${PopulationMonotonicitySpec.relative(dir)}" in {
      withClue("takes decided while a question they read was open, moved by its answer: ")(found(dir)._1.get shouldBe empty)
    }
  }

  slices.foreach { dir =>
    "A real family" should s"decide no take while a sibling's venue page is unread, beyond its cluster: ${PopulationMonotonicitySpec.relative(dir)}" in {
      withClue("takes decided while a family sibling's page, outside their cluster, was unread: ")(found(dir)._2.get shouldBe empty)
    }
  }
}

/** The parts of [[ReadSetClosureSpec]] ([[SliceParts]]): each its own class, so the identity shards spread them. */
class ReadSetClosurePart1Spec extends ReadSetClosureSpec(1)
class ReadSetClosurePart2Spec extends ReadSetClosureSpec(2)
class ReadSetClosurePart3Spec extends ReadSetClosureSpec(3)
class ReadSetClosurePart4Spec extends ReadSetClosureSpec(4)

object ReadSetClosureSpec {

  /** A slice read: its answers replayed through one memo, its listings. Every check of the slice reads this one. */
  final case class Read(dir: Path, reads: IdentitySlice.Reads, lookups: IdentityLookups, all: Seq[Listing])
  object Read {
    def apply(dir: Path): Read = {
      val slice     = IdentitySlice.readSlice(dir)
      val replaying = new tools.SliceHttp.Replaying(slice.http)
      val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed,
        tools.ProdState.none)
      Read(dir, reads, scripts.IdentityDrift.memo(reads), (slice.core ++ slice.context).sorted.distinctBy(_.key))
    }
  }

  /** The rounds a slice's answers are filed in, and the seeds of their order. */
  val Rounds = 4
  val Seeds: Seq[Long] = Seq(1L, 2L)

  /** `inner` with each question whose bucket (of `seed`) is `round` or later unanswered: a search, a venue page, a record. */
  private final class Withheld(inner: IdentityLookups, seed: Long, round: Int) extends ForwardingIdentityLookups(inner) {
    private def open(what: String) = math.floorMod(MurmurHash3.stringHash(s"$seed|$what"), Rounds) >= round
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = if (open(s"q|$query")) Answer.Unknown else inner.candidates(query)
    override def detail(listing: Listing): Answer[Option[DetailFacts]] =
      if (open(s"d|${listing.key}")) Answer.Unknown else inner.detail(listing)
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = if (open(s"f|$tmdbId")) Answer.Unknown else inner.film(tmdbId)
  }

  /** `inner` with the venue pages of `arriving` unread: listings that just arrived, their pages not read yet. */
  private final class Unread(inner: IdentityLookups, arriving: Set[services.movies.ListingKey]) extends ForwardingIdentityLookups(inner) {
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = if (arriving(listing.key)) Answer.Unknown else inner.detail(listing)
  }

  /** Every listing whose take, decided while the listings that just ARRIVED had their venue pages unread, their reading
   *  moved -- along [[PopulationMonotonicity.chains]] of the slice's listings. UK Torrington "Dracula" (#595): Quad
   *  Derby's "Dracula (15)" arrived with its page unread, Torrington's take was decided none, and went back to 11868
   *  once the page was read. */
  def arrivals(read: Read): Seq[String] = {
    import read._
    def takes(listings: Seq[Listing], over: IdentityLookups) = {
      val r = IdentityResolver.resolve(listings, over, reads.normalizer, ResolveInputs.Unpinned)
      listings.map(l => l.key -> r.decisionOf.get(l.key).flatMap(PopulationMonotonicity.takeOf)).toMap
    }
    PopulationMonotonicity.chains(all, MurmurHash3.stringHash(dir.getFileName.toString).toLong, ArrivalChains, ArrivalSteps).zipWithIndex.flatMap {
      case (chain, c) =>
        chain.zip(Nil +: chain).flatMap { case (now, before) =>
          val arrived = now.map(_.key).toSet -- before.map(_.key)
          val (open, read) = (takes(now, new Unread(lookups, arrived)), takes(now, lookups))
          now.map(_.key).flatMap(key => open.get(key).flatten.filter(decided => read.get(key).flatten != Some(decided)).map(decided =>
            s"${key.venue} | ${key.rawTitle}: $decided with ${arrived.size} arrival(s)' pages unread, ${read.get(key).flatten.getOrElse("pending")} once read [chain $c, ${now.size} listings]"))
        }
    }.distinct.sorted
  }

  /** Every listing decided while the venue page of a listing of its FAMILY in another cluster is unread -- the
   *  read-set's own statement (stage 4): a decision reads its family's evidence (the pool, a sibling's denial, the
   *  family majority, the must-link reach), so it awaits every open question of its family, not its cluster's alone. For
   *  each family the whole resolve splits into clusters, one listing of each cluster (at most [[SiblingProbes]]) has its
   *  page withheld; where that leaves its own take awaiting its page, no listing of its family may be decided. Before
   *  stage 4 a take awaited its cluster and its must-links' reach only, and a sibling cluster's listings stayed decided. */
  def unreadSibling(read: Read): Seq[String] = {
    import read._
    val whole     = IdentityResolver.resolve(all, lookups, reads.normalizer, ResolveInputs.Unpinned)
    val probes    = whole.decisions.groupBy(d => whole.familyOf.get(d.members.head)).collect {
      case (Some(_), decisions) if decisions.sizeIs > 1 => decisions.map(_.members.min).sorted.take(SiblingProbes)
    }.flatten.toSeq.sorted
    probes.flatMap { probe =>
      val r = IdentityResolver.resolve(all, new Unread(lookups, Set(probe)), reads.normalizer, ResolveInputs.Unpinned)
      val awaitsItsPage = r.decisionOf.get(probe).exists(_.awaits.contains(Question.VenuePage(probe)))
      val family        = r.familyOf.get(probe)
      if (!awaitsItsPage || family.isEmpty) Nil
      else r.decisions.filter(d => d.members.exists(r.familyOf.get(_) == family) && !d.members.contains(probe))
        .flatMap(d => PopulationMonotonicity.takeOf(d).map(take => s"${d.members.min.venue} | ${d.members.min.rawTitle}: $take with ${probe.venue} | ${probe.rawTitle}'s page unread"))
    }.distinct.sorted
  }

  /** At most how many clusters of one family [[unreadSibling]] withholds a page of. */
  val SiblingProbes = 4

  /** How many arrival chains [[arrivals]] walks, and at most how many steps each. */
  val ArrivalChains = 3
  val ArrivalSteps  = 8

  /** Every listing of the slice in `dir` whose decided take a later round's filing moved, each with its outcomes. */
  def moved(read: Read): Seq[String] = {
    import read._
    Seeds.flatMap { seed =>
      val outcomes = (1 to Rounds).map { round =>
        val r = IdentityResolver.resolve(all, new Withheld(lookups, seed, round), reads.normalizer, ResolveInputs.Unpinned)
        all.map(l => l.key -> r.decisionOf.get(l.key).flatMap(PopulationMonotonicity.takeOf)).toMap
      }
      all.map(_.key).flatMap { key =>
        val seq     = outcomes.map(_.getOrElse(key, None))
        val decided = seq.dropWhile(_.isEmpty)
        Option.when(decided.distinct.sizeIs > 1)(
          s"${key.venue} | ${key.rawTitle}: ${seq.map(_.getOrElse("pending")).mkString(" -> ")} [seed $seed]")
      }
    }.sorted
  }

  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) PopulationMonotonicitySpec.sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach(dir => Try(Read(dir)).flatMap(r => Try(moved(r) ++ arrivals(r))).fold(e => println(s"# ${PopulationMonotonicitySpec.relative(dir)}: FAILED $e"), rows => {
      println(s"# ${PopulationMonotonicitySpec.relative(dir)}: ${rows.size} moved")
      rows.take(8).foreach(r => println(s"    $r"))
    }))
  }
}
