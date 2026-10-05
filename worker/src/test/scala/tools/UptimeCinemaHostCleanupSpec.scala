package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The cleanup deletes `uptimeBuckets` documents whose `service` is a bare cinema
 * scrape host. The risk is collateral damage — wiping the enrichment rows,
 * the per-cinema `displayName` rows, the `img:` reliability rows, or the
 * genuine-external catch-all (Google OAuth) that legitimately live in the
 * uptime page. This pins the selection: it keys off `cinemaHosts` (the same
 * set `MonitoringHttpFetch` suppresses), so only bare hosts match.
 */
class UptimeCinemaHostCleanupSpec extends AnyFlatSpec with Matchers {

  private val hosts = UptimeCinemaHostCleanup.cinemaHosts

  // Mirrors the tool's `before.intersect(hosts)` selection.
  private def selectedForDeletion(existingServices: Set[String]): Set[String] =
    existingServices.intersect(hosts)

  "UptimeCinemaHostCleanup" should "select cinema-host rows for deletion" in {
    val existing = Set("kinomuranow.pl", "amok.gliwice.pl", "www.multikino.pl", "kinoluna.bilety24.pl")
    selectedForDeletion(existing) shouldBe existing
  }

  it should "never delete enrichment, displayName, img or external-catch-all rows" in {
    val keep = Set(
      "TMDB", "IMDb", "Filmweb", "Metacritic", "Rotten Tomatoes",   // enrichment
      "Kino Muranów", "Multikino Stary Browar", "Kino Amok",        // cinema displayNames
      "img: images.weserv.nl", "img: www.multikino.pl",            // poster-load reliability
      "oauth2.googleapis.com", "www.googleapis.com",               // genuine-external catch-all (kept by design)
    )
    selectedForDeletion(keep) shouldBe empty
  }
}
