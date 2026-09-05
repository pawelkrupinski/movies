package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Regression guard for "the bound that multiplies": a cinema client must take
 *  its detail cache from the composition root, never build its own.
 *
 *  `CachingDetailFetch` is bounded per instance, so a cache constructed inside a
 *  client is a budget per VENUE — and the catalog builds 59 of these clients
 *  (36 Bilety24 organisers, 5 Ekobilet venues, 3 NoveKino, and one apiece for
 *  the rest). On 2026-09-05 worker-pl paged `JvmOldGenNearCap` at 99.69% with
 *  1,015 cached detail bodies holding 228 MiB — 73% of its 313 MiB old gen. The
 *  byte bound that followed (@ed06b001c) fixed the UNIT of the bound but not its
 *  multiplicity: 59 clients x 8 MiB is a 472 MiB ceiling on a 313 MiB space,
 *  which is no bound at all. One shared cache is what makes the ceiling a number
 *  that does not grow when Poland gains a venue.
 *
 *  The catalog is the composition root for these and is where the one instance
 *  is allowed to be built. */
class SharedDetailCacheSpec extends AnyFlatSpec with Matchers {

  private val CinemasDirectory: Path = Paths.get("worker/src/main/scala/services/cinemas")

  /** Where the single shared instance is allowed to be constructed. */
  private val CompositionRoot = "CinemaScraperCatalog.scala"

  private def scalaSourcesUnder(root: Path): Seq[Path] =
    Files.walk(root).iterator.asScala.toSeq
      .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".scala"))
      .sortBy(_.toString)

  "A cinema scraper client" should "take its detail cache, never construct one per venue" in {
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
      "These clients build their own detail cache, so the in-heap budget multiplies by venue " +
        s"instead of being one shared bound handed down from $CompositionRoot:\n" +
        offenders.mkString("\n") + "\n"
    ) {
      offenders shouldBe empty
    }
  }

  /** The point is ONE cache, so the composition root may build one — the chain
   *  caches come from the injected `chainDetailCache` seam, not from here. A
   *  second literal construction is the multiplicity creeping back in at the
   *  only place this spec still permits it. */
  it should "leave the composition root building a single shared cache" in {
    val catalog = CinemasDirectory.resolve(CompositionRoot)
    Files.exists(catalog) shouldBe true

    // The venue cache is the one BOUND TO A VAL. The other permitted construction
    // is the `chainDetailCache` seam's diagnostic default, which is a lambda —
    // it builds per chain (two chains, distinct TTLs) and production overrides it
    // with the Mongo-backed cache, so it never holds venue bodies on this heap.
    val BoundToAVal = """\bval\s+\w+\s*(?::[^=]+)?=\s*new CachingDetailFetch\(""".r

    val constructions: Seq[String] =
      Files.readAllLines(catalog, StandardCharsets.UTF_8).asScala.zipWithIndex.collect {
        case (line, index) if BoundToAVal.findFirstIn(line).isDefined => s"  ${index + 1}: ${line.trim}"
      }.toSeq

    withClue(s"Expected exactly one shared venue cache in $CompositionRoot, found:\n${constructions.mkString("\n")}\n") {
      constructions should have size 1
    }
  }
}
