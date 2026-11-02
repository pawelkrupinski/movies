package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.{IdentityDriftSlicesSpec, IdentitySlice}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Clock, ZoneOffset}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * THE POPULATION PROPERTY over REAL families (docs/investigations/2026-11-01-flip-unification.md, stage 1): every
 * committed slice -- the drift slices (`test/resources/fixtures/drift-slices/`, each a listing that once diverged or
 * flipped) and the identity slices (`identity-slices/`, each a pinned case) -- is a recording's family with its corpus
 * context and every answer it reads. Its families are walked along arrival chains ([[PopulationMonotonicity]]): an order
 * of their listings cut into nested prefixes, each resolved whole against the slice's context with the absent listings
 * taken out, every answer filed. No listing's decided take may go back to a value it left (A -> B -> A) as listings only
 * ARRIVE: Addams, Keswick, Prima Facie and Torrington flipped so, with nothing departing.
 *
 * Today's A -> B -> A shapes are KNOWN (`identity-population-flips-known.tsv`), as the flips baseline was on 10-29: a known
 * row passes, a new flip fails, and a known row whose listing no longer flips fails too until its row is removed -- the
 * list only shrinks. Stage 5 of the design (a contested family gives a bare listing no film) is what empties it.
 */
class PopulationMonotonicitySpec extends AnyFlatSpec with Matchers with tools.CountryScope {
  import PopulationMonotonicitySpec._

  override protected def countriesOf(testName: String): Seq[String] =
    if (testName.contains("drift-slices/") || testName.contains("identity-slices/")) Seq(countryOf(Path.of(testName.split(' ').last)))
    else Nil

  private val slices = sliceDirs
  private val known  = Known.read(KnownFile)

  /** Every slice's flips, the slices side by side: each walks its own answers and shares nothing with another. */
  private lazy val found: Map[Path, Try[Seq[Row]]] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try slices.zip(scripts.ParallelTasks.all(slices)(dir => Try(walk(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  // A known row may name a slice another change brings (the #629 Keswick slices land beside this list): it is held once
  // its slice is committed, and said meanwhile, never failed.
  "The known population flips" should "name committed slices, or be said to wait for theirs" in {
    val names = slices.map(relative).toSet
    known.map(_.slice).filterNot(names).distinct.foreach(slice => info(s"known row(s) for a slice not committed yet: $slice"))
    known.map(_.slice).filter(names) should not be empty
  }

  // The corpus report walks families on every core (IdentityResolveDump): side by side, the same findings as one by one.
  "The population walk on several threads" should "find what one thread finds, family for family" in {
    val flipping = known.map(_.slice).distinct.flatMap(name => slices.find(dir => relative(dir) == name)).take(2)
    flipping should not be empty
    flipping.foreach(dir => withClue(s"${relative(dir)}: ")(walk(dir, threads = 3) shouldBe found(dir).get))
  }

  slices.foreach { dir =>
    "A real family" should s"take no listing back to a value it left as its listings arrive: ${relative(dir)}" in {
      val rows  = found(dir).get
      val name  = relative(dir)
      val fresh = rows.filterNot(row => known.exists(_.covers(name, row)))
      val fixed = known.filter(k => k.slice == name && !rows.exists(k.covers(name, _)))
      withClue("new A -> B -> A takes along an arrival chain (add a row to identity-population-flips-known.tsv only with the owner's OK): ")(
        fresh.map(_.line) shouldBe empty)
      withClue("known rows whose listing no longer flips -- remove them from identity-population-flips-known.tsv: ")(
        fixed.map(k => s"${k.venue} | ${k.rawTitle} (${k.note})") shouldBe empty)
    }
  }
}

object PopulationMonotonicitySpec {

  private val Fixtures = Path.of("test", "resources", "fixtures")
  val KnownFile: Path  = Fixtures.resolve("identity-population-flips-known.tsv")

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

  /** One flip found: the listing, its shape and its path along the chain. */
  final case class Row(venue: String, rawTitle: String, shape: String, path: String, chain: Int) {
    def line: String = s"$venue | $rawTitle: $path [chain $chain, $shape]"
  }

  /** A known A -> B -> A take: the slice, the listing, and why it stands. */
  final case class Known(slice: String, venue: String, rawTitle: String, note: String) {
    def covers(name: String, row: Row): Boolean = slice == name && venue == row.venue && rawTitle == row.rawTitle
  }
  object Known {
    def read(file: Path): Seq[Known] =
      if (!Files.exists(file)) Nil
      else Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toSeq.filter(l => l.nonEmpty && !l.startsWith("#") && !l.startsWith("slice\t"))
        .map(_.split("\t", -1)).map {
          case Array(slice, venue, title, note, _*) if note.trim.nonEmpty => Known(slice, venue, title, note)
          case row => sys.error(s"identity-population-flips-known: every row names the slice, venue, raw title and a NOTE: ${row.mkString("|")}")
        }
  }

  /** The slice in `dir` walked: its core's families along [[Chains]] arrival chains, resolved against its own answers. */
  def walk(dir: Path, threads: Int = 1): Seq[Row] = {
    val slice     = IdentitySlice.readSlice(dir)
    val replaying = new tools.SliceHttp.Replaying(slice.http)
    val reads     = new IdentitySlice.Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed,
      tools.ProdState.none)
    val lookups   = scripts.IdentityDrift.memo(reads)
    val all       = slice.core ++ slice.context
    val inputs    = ResolveInputs.Unpinned
    val whole     = IdentityResolver.resolve(all, lookups, reads.normalizer, inputs)
    val core      = slice.core.map(_.key).toSet
    // no budget: every family is walked, so no time is read (a stopwatch that never moves)
    val report    = PopulationMonotonicity.report(all, lookups, reads.normalizer, inputs, whole, new tools.Stopwatch(() => 0L), Chains, Steps,
      budgetSeconds = Double.MaxValue, maxFamily = Int.MaxValue, focus = core, threads = threads)
    report.flips.map(_._2).map(f => Row(f.key.venue, f.key.rawTitle, f.shape, f.path, f.chain)).distinctBy(r => (r.venue, r.rawTitle))
      .sortBy(r => (r.rawTitle, r.venue))
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
        rs.foreach(r => println(s"${relative(dir)}\t${r.venue}\t${r.rawTitle}\t${r.shape}: ${r.path}"))
      })
    }
  }
}
