package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.{IdentityDrift, IdentityDriftSlicesSpec, IdentitySlice}

import java.nio.file.Path
import java.time.{Clock, ZoneOffset}
import services.movies.ListingKey
import scala.util.hashing.MurmurHash3
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * THE POPULATION PROPERTY over REAL families (docs/investigations/2026-11-01-flip-unification.md, stage 1): every
 * committed slice -- the drift slices (`test/resources/fixtures/drift-slices/`, each a listing that once diverged or
 * flipped) and the identity slices (`identity-slices/`, each a pinned case) -- is a recording's family with its corpus
 * context and every answer it reads. Its families' subsets are resolved whole against the slice's context with the
 * absent listings taken out, every answer filed, in the MODEL's take and in the FINAL one (the agreement stage settled
 * over it: what is served) -- a family of at most 7 listings over EVERY subset (every arrival order at once), a larger one
 * along seeded arrival chains ([[walk]]). No listing's decided take may go back to a value it left (A -> B -> A) as
 * listings only ARRIVE: Addams, Keswick, Prima Facie and Torrington flipped so, with nothing departing.
 *
 * Today's A -> B -> A shapes are KNOWN (the `population` rows of `identity-flip-backlog.tsv`), as the flips baseline was on 10-29: a known
 * row passes, a new flip fails, and a known row whose listing no longer flips fails too until its row is removed -- the
 * list only shrinks. Stage 5 of the design (a contested family gives a bare listing no film) is what empties it.
 *
 * THE LANDING CHECK JUDGES IT AGAINST THE CHANGE'S BASE (convergence-reds root cause 2, fix C): with ScalaTest's
 * config map entry [[CandidatesKey]] (the pre-push hook's identity check sets it), a slice whose flips differ from the
 * known rows does not fail here: its rows are written to that directory ([[Candidate]]) and
 * scripts/ci/population-walk-diff walks the same slice with the base's code. Only a flip the base does not have (the
 * listing, the slice and the arrival chain named), or a known row the change cures, refuses the push; what main flips
 * too is main's, said and passed. Without the entry (Main's rows, a laptop run) the known rows judge as above.
 */
abstract class PopulationMonotonicitySpec(part: Int) extends AnyFlatSpec with Matchers with tools.CountryScope {
  import PopulationMonotonicitySpec._

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("drift-slices/") || testName.contains("identity-slices/")) Seq(countryOf(Path.of(testName.split(' ').last)))
    else Nil

  // this part's slices ([[SliceParts]]): the spec is PopulationMonotonicityPart<k>Spec, k = 1 to SliceParts.Count
  private val slices = SliceParts.of(part, sliceDirs)

  // the landing check's candidates directory ([[CandidatesKey]]), read off each test's config map
  @volatile private var candidates: Option[Path] = None
  override def withFixture(test: NoArgTest): org.scalatest.Outcome = {
    candidates = test.configMap.getOptional[String](CandidatesKey).map(Path.of(_))
    super.withFixture(test)
  }
  private val known  = Known.read(KnownFile)

  // the two flipping slices the walk on several threads compares (of every part's slices), and this part's of them
  private val flipping = known.map(_.slice).distinct.flatMap(name => sliceDirs.find(dir => relative(dir) == name)).take(2)
  private[identity] val flippingHere = flipping.filter(slices.contains)

  /** Every slice's flips, the slices side by side: each walks its own answers and shares nothing with another. The
   *  slices in the change's countries only (a test of another country cancels before it reads its slice's result), and
   *  the flipping ones the walk on several threads compares. */
  private lazy val found: Map[Path, Try[Seq[Row]]] = {
    val inScope = SliceParts.inScope(slices, countriesInScope).toSet
    val dirs    = slices.filter(dir => inScope(dir) || flippingHere.contains(dir))
    val pool    = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try dirs.zip(scripts.ParallelTasks.all(dirs)(dir => Try(walk(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  if (SliceParts.holdsWholeChecks(part)) {
    // A known row may name a slice another change brings (the #629 Keswick slices land beside this list): it is held once
    // its slice is committed, and said meanwhile, never failed.
    "The known population flips" should "name committed slices, or be said to wait for theirs" in {
      val names = sliceDirs.map(relative).toSet
      known.map(_.slice).filterNot(names).distinct.foreach(slice => info(s"known row(s) for a slice not committed yet: $slice"))
      known.map(_.slice).filter(names) should not be empty
    }

    "The population walk on several threads" should "have flipping slices to compare" in {
      flipping should not be empty
    }
  }

  // The corpus report walks families on every core (IdentityResolveDump): side by side, the same findings as one by one.
  if (flippingHere.nonEmpty)
    "The population walk on several threads" should s"find what one thread finds, family for family (part $part)" in {
      flippingHere.foreach(dir => withClue(s"${relative(dir)}: ")(walk(dir, threads = 3) shouldBe found(dir).get))
    }

  slices.foreach { dir =>
    "A real family" should s"take no listing back to a value it left as its listings arrive: ${relative(dir)}" in {
      val rows  = found(dir).get
      val name  = relative(dir)
      val fresh = rows.filterNot(row => known.exists(_.covers(name, row)))
      val fixed = known.filter(k => k.slice == name && !rows.exists(k.covers(name, _)))
      candidates.filter(_ => fresh.nonEmpty || fixed.nonEmpty).foreach { dir =>
        val file = Candidate.write(dir, name, rows, known.filter(_.slice == name))
        info(s"${fresh.size} new and ${fixed.size} cured row(s) against the known list: judged against the change's base (${file.getFileName})")
      }
      if (candidates.isEmpty) {
        withClue("new A -> B -> A takes along an arrival chain (add a population row to identity-flip-backlog.tsv only with the owner's OK): ")(
          fresh.map(_.line) shouldBe empty)
        withClue("known rows whose listing no longer flips -- remove them from identity-flip-backlog.tsv: ")(
          fixed.map(k => s"${k.venue} | ${k.rawTitle} (${k.note})") shouldBe empty)
      }
    }
  }
}

/** The parts of [[PopulationMonotonicitySpec]] ([[SliceParts]]): each its own class, so the identity shards spread them. */
class PopulationMonotonicityPart1Spec extends PopulationMonotonicitySpec(1)
class PopulationMonotonicityPart2Spec extends PopulationMonotonicitySpec(2)
class PopulationMonotonicityPart3Spec extends PopulationMonotonicitySpec(3)
class PopulationMonotonicityPart4Spec extends PopulationMonotonicitySpec(4)

object PopulationMonotonicitySpec {

  private val Fixtures = Path.of("test", "resources", "fixtures")
  val KnownFile: Path  = Fixtures.resolve("identity-flip-backlog.tsv")

  /** How many arrival chains each slice's families walk, and at most how many steps each. */
  val Chains = 8
  val Steps  = 8

  /** Every committed slice: the drift slices and the identity slices. */
  def sliceDirs: Seq[Path] =
    IdentityDriftSlicesSpec.sliceDirs(Fixtures.resolve("drift-slices")) ++ IdentityDriftSlicesSpec.sliceDirs(Fixtures.resolve("identity-slices"))

  def relative(dir: Path): String = Fixtures.relativize(dir).toString

  /** A slice's country: its drift-slices/<cc>/ folder, or its identity slice's `<cc>-` prefix. */
  def countryOf(dir: Path): String = {
    val parts = dir.iterator().asScala.map(_.toString).toSeq
    val at    = parts.indexOf("drift-slices")
    if (at >= 0 && at + 1 < parts.size) parts(at + 1) else parts.last.takeWhile(_ != '-')
  }

  /** The config map entry naming the landing check's candidates directory (the class comment). */
  val CandidatesKey = "identity.population.candidates"

  /** A slice the landing check judges against its base: `<dir>/<slice, '/' as '__'>.tsv`, tab-separated -- `slice <name>`,
   *  then a `head <venue> <raw title> <line>` row per flip the change's walk found and a `known <venue> <raw title> <note>`
   *  row per known row of the slice. scripts/ci/population_walk_diff.py reads it. */
  object Candidate {
    def write(dir: Path, slice: String, rows: Seq[Row], known: Seq[Known]): Path = {
      def clean(s: String) = s.replace('\t', ' ').replace('\n', ' ')
      val lines = s"slice\t$slice" +: (rows.map(r => s"head\t${clean(r.venue)}\t${clean(r.rawTitle)}\t${clean(r.line)}") ++
        known.map(k => s"known\t${clean(k.venue)}\t${clean(k.rawTitle)}\t${clean(k.note)}"))
      Files.createDirectories(dir)
      val file = dir.resolve(slice.replace("/", "__") + ".tsv")
      Files.write(file, lines.asJava, StandardCharsets.UTF_8)
    }
  }

  /** One flip found: the listing, its shape, the take it flips in (`model`, or `final`: the model then the agreement
   *  stage settled -- what is served), its path, and where the walk found it (`chain <k>`, or `lattice 2^<n>`). */
  final case class Row(venue: String, rawTitle: String, shape: String, path: String, stage: String, where: String) {
    def line: String = s"$venue | $rawTitle: $path [$stage, $where, $shape]"
  }

  /** A known A -> B -> A take: the slice, the listing, and why it stands. */
  final case class Known(slice: String, venue: String, rawTitle: String, note: String) {
    def covers(name: String, row: Row): Boolean = slice == name && venue == row.venue && rawTitle == row.rawTitle
  }
  object Known {
    /** The backlog's `population` rows (scripts.IdentityDrift.backlog). */
    def read(file: Path): Seq[Known] = IdentityDrift.backlog(file, "population").map { r =>
      if (r("slice").trim.isEmpty || r("note").trim.isEmpty)
        sys.error(s"identity-flip-backlog: every population row names the slice, venue, raw title and a NOTE: ${r.values.mkString("|")}")
      Known(r("slice"), r("venue"), r("rawTitle"), r("note"))
    }
  }

  /**
   * The slice in `dir` walked, against its own answers: each family of its core, in TWO takes -- the MODEL's
   * ([[IdentityResolver.resolve]]) and the FINAL one (the model, then a fresh agreement stage settled to its fixpoint,
   * [[scripts.IdentityDrift.whole]]: what is served; Lichtblick "Buddies" and Grünstadt "Titans" flip in it alone).
   *
   *  - a family of at most [[PopulationMonotonicity.ExhaustiveMax]] listings: EXHAUSTIVELY, every subset of its listings
   *    resolved whole with the rest of the slice present ([[PopulationMonotonicity.latticeFlip]]: every arrival order at
   *    once; Buddies is clean on every seed and flips on its 8 subsets);
   *  - a larger one: [[Chains]] seeded arrival chains of at most [[Steps]] steps, the model's against the slice's context
   *    with the absent listings taken out ([[PopulationMonotonicity.report]]), the final one with the rest present (the
   *    nightly explorer walks them further).
   */
  def walk(dir: Path, threads: Int = 1): Seq[Row] = {
    val slice     = IdentitySlice.readSlice(dir)
    val replaying = new tools.SliceHttp.Replaying(slice.http)
    def readsAt(clock: Clock) = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, clock, slice.filed, tools.ProdState.none)
    val reads     = readsAt(Clock.fixed(slice.recordedAt, ZoneOffset.UTC))
    val lookups   = scripts.IdentityDrift.memo(reads)
    val all       = (slice.core ++ slice.context).sorted.distinctBy(_.key)
    val inputs    = ResolveInputs.Unpinned
    val whole     = IdentityResolver.resolve(all, lookups, reads.normalizer, inputs)
    val core      = slice.core.map(_.key).toSet
    val families  = all.groupBy(l => whole.familyOf.getOrElse(l.key, -1)).toSeq
      .filter { case (id, ls) => id >= 0 && ls.sizeIs >= 2 && ls.exists(l => core(l.key)) }.sortBy(_._1).map(_._2)
    val (small, large) = families.partition(_.sizeIs <= PopulationMonotonicity.ExhaustiveMax)
    val subsets   = new PopulationMonotonicity.Subsets(all, lookups, reads.normalizer, inputs)
    type Takes    = Map[ListingKey, Option[String]]

    // both takes of `present` of `family`'s listings, the rest of the slice fixed ([[PopulationMonotonicity.Subsets]]): the
    // model's, and the final one -- the model's resolve, then a fresh stage settled over it (Pending: None)
    def bothTakes(family: Seq[Listing], present: Seq[Listing]): (Takes, Takes) = {
      val model = subsets.resolve(family.map(_.key).toSet, present)
      val clock = new tools.MutableClock(slice.recordedAt)
      val r     = scripts.IdentityDrift.whole(readsAt(clock), present, clock, scripts.IdentityDrift.WholeLookups(lookups, _ => None),
        resolved = Some(model), log = false)
      val pend  = scripts.IdentityDrift.pendingOf(r.decisions)
      (present.flatMap(l => model.decisionOf.get(l.key).map(d => l.key -> PopulationMonotonicity.takeOf(d))).toMap,
        scripts.IdentityDrift.takesOf(r).map { case (k, t) => k -> Option.when(!pend(k))(t.film) })
    }
    // every family's subsets resolved, side by side on `threads` (each resolve builds its own scope and stage)
    def resolved[A](jobs: Seq[A])(run: A => (Takes, Takes)): Map[A, (Takes, Takes)] =
      if (threads <= 1) jobs.map(j => j -> run(j)).toMap
      else {
        val pool = java.util.concurrent.Executors.newFixedThreadPool(threads)
        try jobs.zip(scripts.ParallelTasks.all(jobs)(run)(using scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
        finally pool.shutdown()
      }
    val stages = Seq[(String, ((Takes, Takes)) => Takes)]("model" -> (_._1), "final" -> (_._2))

    // the small families: every subset
    val lattice: Seq[Row] = {
      val jobs  = small.indices.flatMap(f => PopulationMonotonicity.subsets(small(f).size).filter(_ != 0).map(f -> _))
      val takes = resolved(jobs) { case (f, mask) => bothTakes(small(f), small(f).indices.filter(i => (mask & (1 << i)) != 0).map(small(f))) }
      small.zipWithIndex.flatMap { case (members, f) =>
        def label(mask: Int) = members.indices.filter(i => (mask & (1 << i)) != 0).map(members(_).venue).mkString("{", ", ", "}")
        for {
          (stage, pick) <- stages
          (member, own) <- members.zipWithIndex
          witness       <- PopulationMonotonicity.latticeFlip(members.size, own, mask => pick(takes((f, mask))).get(member.key).flatten)
        } yield {
          val flip = PopulationMonotonicity.Flip(member.key, -1, witness.map { case (m, t) => Integer.bitCount(m) -> Some(t) })
          Row(member.venue, member.rawTitle, flip.shape, witness.map { case (m, t) => s"$t@${label(m)}" }.mkString(" -> "), stage,
            s"lattice 2^${members.size}")
        }
      }
    }

    // the large families: seeded arrival chains (the report's: its seed, its cut), both takes along each
    val chained: Seq[Row] = {
      val chains = large.indices.map(f => PopulationMonotonicity.chains(large(f), MurmurHash3.stringHash(large(f).head.sortKey).toLong, Chains, Steps))
      // a chain's prefix of a size is one subset per chain: keyed by family, chain and size
      val keyed  = chains.indices.flatMap(f => chains(f).zipWithIndex.flatMap { case (chain, c) => chain.map(prefix => (f, c, prefix.size) -> prefix) }).toMap
      val takes  = resolved(keyed.keys.toSeq.sorted) { case (f, c, n) => bothTakes(large(f), keyed((f, c, n))) }
      for {
        (stage, pick)  <- stages
        f              <- large.indices
        (chain, c)     <- chains(f).zipWithIndex
        member         <- large(f)
        sequence        = chain.flatMap(prefix => pick(takes((f, c, prefix.size))).get(member.key).map(prefix.size -> _))
        if PopulationMonotonicity.returns(sequence.map(_._2))
      } yield {
        val flip = PopulationMonotonicity.Flip(member.key, c, sequence)
        Row(member.venue, member.rawTitle, flip.shape, flip.path, stage, s"chain $c")
      }
    }

    (lattice ++ chained).distinctBy(r => (r.venue, r.rawTitle)).sortBy(r => (r.rawTitle, r.venue))
  }

  /** `PopulationMonotonicitySpec [slice dir...]`: each slice's flips as TSV rows of the known file (every slice without
   *  arguments) -- what a known row is written from. */
  def main(args: Array[String]): Unit = {
    val dirs = if (args.isEmpty) sliceDirs else args.toSeq.map(Path.of(_))
    dirs.foreach { dir =>
      val started = System.nanoTime()
      val rows    = Try(walk(dir))
      val seconds = (System.nanoTime() - started) / 1e9
      rows.fold(e => println(s"# ${relative(dir)}: FAILED $e"), rs => {
        println(f"# ${relative(dir)}: ${rs.size} flip(s) in $seconds%.1fs")
        rs.foreach(r => println(s"${relative(dir)}\t${r.venue}\t${r.rawTitle}\t${r.shape}: ${r.stage} ${r.path} [${r.where}]"))
      })
    }
  }
}
