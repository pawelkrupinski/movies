package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * Regression guard: a venue client does not cache its detail pages, and none of
 * them may start again by building a cache of its own.
 *
 * BOTH HALVES OF THIS WERE LEARNED THE EXPENSIVE WAY. `CachingDetailFetch` was
 * added 2026-06-06 because the slow scrapers pulled every film's detail page
 * inline in `fetch()`, on minutes-apart passes — a real problem, correctly
 * solved. Within 48 hours every one of those clients moved to deferred queue
 * detail and nothing re-read a detail URL inside a pass again, but each client
 * kept its own cache. Three months later that was 59 instances (36 Bilety24
 * organisers alone), bounded by ENTRY COUNT over whole HTML pages, holding
 * 1,015 bodies worth 228 MiB — 73% of worker-pl's old generation, and the
 * JvmOldGenNearCap page of 2026-09-05.
 *
 * What finally removed it was arithmetic rather than tuning. Detail is fetched
 * once per film per `FreshnessKind.DetailEnrich` window (6h) and the cache TTL
 * expires an hour in, so a scheduled refresh can never be served from it; a
 * durably-gone page is stamped and backs off to the same window; a transient
 * failure was never cached at all. The one population left was a film stuck in
 * the `DetailFetchOutcome.Failed` livelock, re-enqueued every reaper tick
 * forever — and that is fixed at the clients now
 * ([[DetailEnricherDurableFailureSpec]]), not absorbed here. Nothing reads a
 * venue detail cache, so there is none.
 *
 * The chains are the exception and keep theirs: `HeliosClient` fetches its movie
 * and screen bodies from `fetchRestData()`, INSIDE the scrape pass, for every id
 * in the listing — the original per-pass redundancy, still real. That cache is
 * injected through `CinemaScraperCatalog`'s `chainDetailCache` seam and is
 * Mongo-backed in production.
 */
class SharedDetailCacheSpec extends AnyFlatSpec with Matchers {

  private val CinemasDirectory: Path = Paths.get("worker/src/main/scala/services/cinemas")

  /** The composition root: the only place allowed to name a detail cache at all,
   *  and there only as the `chainDetailCache` seam's diagnostic default. */
  private val CompositionRoot = "CinemaScraperCatalog.scala"

  private def scalaSourcesUnder(root: Path): Seq[Path] =
    Files.walk(root).iterator.asScala.toSeq
      .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".scala"))
      .sortBy(_.toString)

  "A cinema scraper client" should "never construct a detail cache of its own" in {
    Files.exists(CinemasDirectory) shouldBe true

    val offenders: Seq[String] =
      scalaSourcesUnder(CinemasDirectory)
        .filterNot(_.getFileName.toString == CompositionRoot)
        .flatMap { path =>
          Files.readAllLines(path, StandardCharsets.UTF_8).asScala.zipWithIndex.collect {
            case (line, index) if line.contains("new CachingDetailFetch(") =>
              s"  ${CinemasDirectory.relativize(path)}:${index + 1}: ${line.trim}"
          }
        }

    withClue(
      "These clients build their own detail cache. A per-client cache is a heap budget per VENUE, " +
        "and the catalog builds 59 of them — which is how worker-pl came to hold 228 MiB of HTML. " +
        s"A chain that genuinely needs one takes it from $CompositionRoot's chainDetailCache seam:\n" +
        offenders.mkString("\n") + "\n"
    ) {
      offenders shouldBe empty
    }
  }

  /** The venue cache was a `val` on the catalog; the chain seam's default is a
   *  lambda. So "no detail cache is bound to a name here" is exactly the
   *  statement that the venue-wide cache has not come back. */
  it should "leave the composition root holding no detail cache of its own" in {
    val catalog = CinemasDirectory.resolve(CompositionRoot)
    Files.exists(catalog) shouldBe true

    val BoundToAVal = """\bval\s+\w+\s*(?::[^=]+)?=\s*new CachingDetailFetch\(""".r
    val bound: Seq[String] =
      Files.readAllLines(catalog, StandardCharsets.UTF_8).asScala.zipWithIndex.collect {
        case (line, index) if BoundToAVal.findFirstIn(line).isDefined => s"  ${index + 1}: ${line.trim}"
      }.toSeq

    withClue(
      "A detail cache is held on the catalog again. Nothing reads one: detail is fetched once per " +
        s"DetailEnrich window and any TTL short enough to be correct expires first.\n${bound.mkString("\n")}\n"
    ) {
      bound shouldBe empty
    }
  }
}
