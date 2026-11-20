package scripts

import services.identity.{InMemoryIdentityModelStore, IncrementalResolver, Listing, OlderRefs}

import java.nio.file.{Files, Path, Paths}
import tools.SliceHttp
import java.time.{Clock, ZoneOffset}
import scala.jdk.CollectionConverters.*

/**
 * WHAT THE IDENTITY MODEL ALLOCATES AND SPENDS PER FAMILY (scripts/identity-boot-cost; its ratchet: scripts/
 * identity_boot_cost.py over test/resources/identity-boot-cost-baseline.tsv), over the pinned identity slices -- real
 * listings and every answer they read (`test/resources/fixtures/identity-slices/`, `IdentitySliceCasesSpec`'s), decided
 * hermetically from their own answers. Three phases, each in a model of its own over the slice's every listing:
 *
 *   resolve      a cold model takes every listing in (`batch`): the resolver's hot paths, a family at a time
 *   boot         a model restarted over the first's store under OTHER rules (`restore`, `rules` changed): every family
 *                re-resolved -- the worker's take-up after a deploy that moved the rules, the ~17-20 MB a family a GC
 *                investigation saw in production on 2026-11-20, visible only after deploy until now
 *   boot-steady  the same restart under the SAME rules: what a take-up with nothing moved pays (its stored families
 *                stand)
 *
 * Each phase is measured `runs` times after one unmeasured warm-up pass of all slices (which also fills each slice's
 * replayed lookups, so a phase pays the resolver's work, not the answers' first parse), on this thread only
 * (`tools.ThreadAllocation`: the take-up runs on one thread here, `takeUpThreads` 1), and a slice's figure is the
 * MEDIAN of its runs. Run it in a JVM without escape analysis (scripts/identity-boot-cost does) so the bytes are the
 * code's, not what C2 managed to scalar-replace (ResolverAllocationSpec, "Measured ONLY in a JVM without escape
 * analysis").
 *
 *   sbt "worker/Test/runMain scripts.IdentityBootCost <out.tsv> [runs] [slices-dir]"
 *
 * writes `phase<TAB>slice<TAB>families<TAB>bytes<TAB>nanos` a slice and phase, then `phase<TAB>*<TAB>families<TAB>
 * bytes<TAB>nanos` the phase's totals.
 */
object IdentityBootCost {

  final case class Figure(families: Int, bytes: Long, nanos: Long)

  val Phases: Seq[String] = Seq("resolve", "boot", "boot-steady")

  /** A pinned slice's listings and the model's inputs, read once. */
  private final class Pinned(dir: Path) {
    private val slice   = IdentitySlice.readSlice(dir)
    private val replay  = new SliceHttp.Replaying(slice.http, None)
    private val reads   = new IdentitySlice.Reads(slice.country, replay, replay.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed,
      tools.ProdState.none)
    private val (calibration, pins, decorations) = OlderRefs.resolveInputs(reads.prod.pins)
    val name: String            = dir.getFileName.toString
    val listings: Seq[Listing]  = slice.core ++ slice.context

    def model(store: InMemoryIdentityModelStore, rules: String): IncrementalResolver =
      new IncrementalResolver(reads.lookups, reads.normalizer, calibration, pins, decorations, store = store, rules = rules)

    /** Each phase's figure, one pass: a cold resolve, then the two restarts over its store. */
    def pass(): Map[String, Figure] = {
      val store = new InMemoryIdentityModelStore
      val first = model(store, "rules-a")
      val resolve = measure(first.batch(seen = listings), first.familyCount)
      val again   = model(store, "rules-b")
      val boot    = measure(again.restore(listings), again.familiesResolved)
      val steady  = model(store, "rules-b")
      val stand   = measure(steady.restore(listings), steady.familyCount)
      Map("resolve" -> resolve, "boot" -> boot, "boot-steady" -> stand)
    }

    private def measure(body: => Unit, families: => Int): Figure = {
      val started = System.nanoTime
      val (_, bytes) = tools.ThreadAllocation.of(body)
      Figure(families, bytes, System.nanoTime - started)
    }
  }

  private def median(xs: Seq[Long]): Long = xs.sorted.apply(xs.size / 2)

  def main(args: Array[String]): Unit = {
    require(args.nonEmpty, "IdentityBootCost <out.tsv> [runs] [slices-dir]")
    require(tools.ThreadAllocation.supported, "this JVM cannot measure a thread's allocation")
    val out   = Paths.get(args(0))
    val runs  = args.lift(1).map(_.toInt).getOrElse(3)
    val root  = Paths.get(args.lift(2).getOrElse("test/resources/fixtures/identity-slices"))
    val dirs  = Files.list(root).iterator().asScala.filter(d => Files.exists(d.resolve("expected.tsv"))).toSeq.sortBy(_.getFileName.toString)
    val slices = dirs.map(new Pinned(_))
    val started = System.nanoTime
    slices.foreach(_.pass())   // the warm-up: classes loaded, the JIT's first tiers, each slice's lookups filled
    val passes = slices.map(s => s -> (1 to runs).map(_ => s.pass()))
    val rows = for {
      phase       <- Phases
      (s, ofRuns) <- passes
    } yield {
      val figures = ofRuns.map(_(phase))
      phase -> (s.name, Figure(figures.head.families, median(figures.map(_.bytes)), median(figures.map(_.nanos))))
    }
    val lines = rows.map { case (phase, (slice, f)) => s"$phase\t$slice\t${f.families}\t${f.bytes}\t${f.nanos}" } ++
      Phases.map { phase =>
        val of = rows.collect { case (p, (_, f)) if p == phase => f }
        s"$phase\t*\t${of.map(_.families).sum}\t${of.map(_.bytes).sum}\t${of.map(_.nanos).sum}"
      }
    Files.write(out, lines.asJava)
    println(s"IdentityBootCost: ${slices.size} slices, $runs runs a phase, ${(System.nanoTime - started) / 1000000} ms, to $out")
  }
}
