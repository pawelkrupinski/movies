package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Regression guard for the retired Trakt (api.trakt.tv) integration.
 *
 *  Trakt was an id-crosswalk SOURCE: a keyed client whose search results carry
 *  `imdb` + `tmdb` together, wired as a fallback rung in `resolveTmdbId` and
 *  `ImdbIdResolver`. It was retired because it stopped paying for itself —
 *  `docs/white-cinema-investigations.md` measured its entire addressable
 *  population at 6 films (0.16% of the corpus), none of which it actually
 *  resolved, while its credential had been 403-ing behind Cloudflare since
 *  Dec 2025. Letterboxd occupies the same niche (imdbId → tmdbId, no key) and
 *  is already the rung that follows it.
 *
 *  This guard is STRUCTURAL rather than behavioural on purpose. The client was
 *  feature-gated on `TRAKT_API_CLIENT_ID`, so in every test environment (where
 *  the key is unset) it already made no HTTP call and no-opped — there is no
 *  observable runtime difference between "wired but keyless" and "removed" for
 *  a behavioural spec to assert. What IS observable, and what actually
 *  regresses if someone reintroduces the dependency, is the source tree. Same
 *  shape as `NoWallClockInClientsSpec`.
 */
class NoTraktIntegrationSpec extends AnyFlatSpec with Matchers {

  private val ScannedRoots: Seq[Path] =
    Seq("worker/src/main/scala", "common/src/main/scala", "web/src/main/scala").map(Paths.get(_))

  private val TraktReference = """(?i)trakt""".r

  "The worker" should "carry no Trakt integration — client, resolver, credential or host" in {
    ScannedRoots.foreach(root => withClue(s"$root should exist: ")(Files.exists(root) shouldBe true))

    val offenders: Seq[String] =
      ScannedRoots.flatMap { root =>
        Files.walk(root).iterator.asScala.toSeq
          .filter(p => p.getFileName.toString.endsWith(".scala"))
          .sortBy(_.toString)
          .flatMap { path =>
            Files.readAllLines(path).asScala.zipWithIndex.collect {
              case (line, index) if TraktReference.findFirstIn(line).isDefined =>
                s"$path:${index + 1}: ${line.trim}"
            }
          }
      }

    withClue(
      "Trakt was retired (see the scaladoc above). These lines reintroduce it — client, resolver, " +
        "TRAKT_* credential, api.trakt.tv host, or a comment that still claims Trakt is a rung:\n" +
        offenders.mkString("\n") + "\n"
    ) {
      offenders shouldBe empty
    }
  }
}
