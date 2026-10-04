package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, code, read, scalaFiles}

import java.nio.file.{Files, Paths}

/**
 * Caches, queues and pools are built bounded, through their factories.
 *
 * The class of bug: a structure that only grows. A Caffeine cache with no size bound, or one whose
 * weigher answered 0 (never evicted); a `new LinkedBlockingQueue()` (capacity `Int.MaxValue`) behind
 * a trace writer or a pool; an `Executors.newFixedThreadPool` / `newSingleThreadExecutor`, which
 * queue without bound — each a producer outrunning its consumer until the heap was gone, and each
 * fixed in its own place. The bound now lives in two factories, and these shapes outside them fail
 * the build:
 *
 *  - `Caffeine.newBuilder` — use [[BoundedCache.ofSize]] / [[BoundedCache.ofWeight]];
 *  - `Executors.new…`, `new ThreadPoolExecutor`, `new ScheduledThreadPoolExecutor` — use
 *    [[DaemonExecutors]] (`boundedPool`, `scheduler`, `virtualThreadEC`, `boundedEC`, …);
 *  - `new LinkedBlockingQueue`/`LinkedBlockingDeque` without a capacity.
 *
 * Where unbounded is right, add the site to [[Allowlist]] with WHY.
 */
class NoUnboundedConcurrencyPrimitiveSpec extends AnyFlatSpec with Matchers {

  private val Factories = Set(
    "common/src/main/scala/tools/BoundedCache.scala",
    "common/src/main/scala/tools/DaemonExecutors.scala")

  private val Shapes = Seq(
    """\bCaffeine\s*\.\s*newBuilder\b""".r                                   -> "build it through tools.BoundedCache",
    """\bExecutors\s*\.\s*new\w+""".r                                        -> "build it through tools.DaemonExecutors",
    """\bnew\s+(?:Scheduled)?ThreadPoolExecutor\b""".r                       -> "build it through tools.DaemonExecutors",
    """\bnew\s+LinkedBlocking(?:Queue|Deque)\s*(?:\[[^\]]*\])?+\s*+(?:\(\s*\))?+(?!\()""".r -> "give the queue a capacity")

  /** (repository-relative file, the flagged line trimmed) → why unbounded is right there. */
  private val Allowlist: Map[(String, String), String] = Map(
    ("common/src/main/scala/services/movies/MovieCache.scala",
      "private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().recordStats().build()") ->
      ("the resident corpus, not a cache: every write funnels through it and CorpusIndex shadows it with no eviction " +
        "path, so an evicted row would desync the index and drop a film from the scrape fold; its size is the corpus's"))

  private[tools] def offenders(path: String, source: String): Seq[(Int, String, String)] =
    if (Factories(path)) Nil
    else source.linesIterator.zipWithIndex.toSeq.flatMap { case (line, index) =>
      val live = code(line)
      Shapes.collectFirst { case (shape, fix) if shape.findFirstIn(live).isDefined => (index + 1, line.trim, fix) }
    }

  private lazy val found: Seq[(String, Int, String, String)] = for {
    file                <- scalaFiles(MainRoots)
    (line, text, fix)   <- offenders(file.toString, read(file))
  } yield (file.toString, line, text, fix)

  "Main sources" should "build caches, queues and pools only through their bounded factories, outside the allowlist" in {
    MainRoots.map(Paths.get(_)).foreach(root => withClue(s"$root must exist (run from the repo root)")(Files.isDirectory(root) shouldBe true))
    val unexplained = found.filterNot { case (file, _, text, _) => Allowlist.contains(file -> text) }
    withClue(unexplained.map { case (file, line, text, fix) => s"$file:$line: $text — $fix" }.mkString(
      "\nBound each of these through its factory, or, where unbounded is right, add (file, trimmed line) -> WHY to Allowlist:\n",
      "\n", "\n"))(unexplained shouldBe empty)
  }

  "The allowlist" should "name only sites that still exist" in {
    val present = found.map { case (file, _, text, _) => file -> text }.toSet
    withClue("No longer in the source — drop from Allowlist: ")((Allowlist.keySet -- present) shouldBe empty)
  }

  "The lint" should "flag each shape, and nothing in a comment, a factory, or a bounded queue" in {
    val src =
      """val a = Caffeine.newBuilder().build()
        |val b = Executors.newFixedThreadPool(4)
        |val c = new LinkedBlockingQueue[Runnable]()
        |val d = new LinkedBlockingQueue[Runnable](64)
        |// Caffeine.newBuilder() in a comment
        |val e = new ThreadPoolExecutor(1, 1, 0L, unit, queue)
        |val f = BoundedCache.ofSize(10).build()
        |val g = new LinkedBlockingDeque[Int]
        |val h = java.util.concurrent.Executors.newSingleThreadScheduledExecutor()
        |""".stripMargin
    offenders("worker/src/main/scala/A.scala", src).map(_._1) shouldBe Seq(1, 2, 3, 6, 8, 9)
    offenders("common/src/main/scala/tools/DaemonExecutors.scala", src) shouldBe empty
  }
}
