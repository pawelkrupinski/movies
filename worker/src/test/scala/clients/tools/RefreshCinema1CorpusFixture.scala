package clients.tools

import services.cinemas.pl.Cinema1Client

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.LocalDate

/**
 * One-off, narrowly-scoped refresh of ONLY the Cinema1 slice of the frozen
 * `08-06-2026` whole-corpus fixture, after Cinema1's 2026 platform migration
 * (see `Cinema1Client`'s scaladoc) replaced the old MSI-portal HTML with a
 * JSON REST API.
 *
 * Unlike a normal re-record, this does NOT re-fetch with the corpus's pinned
 * `today` (2026-06-08, parsed from the directory name by `FixtureTestWiring`):
 * `Cinema1Client`'s screenings query is a `dateTimeFrom`/`dateTimeTo` WINDOW,
 * and by 2026-09 (when this venue's migration was discovered and fixed) that
 * date is three months in the real past — querying the live API with it
 * returns not just upcoming screenings but every screening the venue has
 * ALREADY run since then (1311 rows vs. the ~185 a real one-time capture
 * would see), which would bloat this one cinema's corpus slice far beyond
 * what every other cinema's fixture looks like for no benefit (our own
 * client never queries a past-anchored window in production).
 *
 * Instead: reuse the already-recorded, correctly-shaped `cinema1-gdansk`
 * client-level fixture (real API responses, captured with `today` genuinely
 * current) and re-file its screenings response under the query-fingerprinted
 * filename the corpus replay will actually request (`today=2026-06-08`) —
 * same real content, just keyed for the frozen corpus's pinned clock. The
 * movie-detail and screenhead fixtures need no such treatment since their
 * URLs carry no date-dependent query.
 */
object RefreshCinema1CorpusFixture {

  private val CinemaId = "8d3b10d9-f892-4f57-bf74-9f86905ce3ea"

  def main(args: Array[String]): Unit = {
    val sourceRoot = Paths.get("test/resources/fixtures/cinema1-gdansk/restapi.cinemaone.pl")
    val targetRoot = Paths.get("test/resources/fixtures/08-06-2026/restapi.cinemaone.pl")

    // Movie + screenhead fixtures: no date-dependent query, copy verbatim.
    Files.createDirectories(targetRoot.resolve(s"api/cinema/$CinemaId"))
    Files.createDirectories(targetRoot.resolve("api/movie"))
    copy(sourceRoot.resolve(s"api/cinema/$CinemaId/screenhead"), targetRoot.resolve(s"api/cinema/$CinemaId/screenhead"))
    val movieDir = sourceRoot.resolve("api/movie")
    Files.list(movieDir).forEach { movieFile =>
      copy(movieFile, targetRoot.resolve("api/movie").resolve(movieDir.relativize(movieFile)))
    }

    // Screenings: re-file under the fingerprint the corpus replay's pinned
    // `today` (2026-06-08) computes, per the scaladoc above.
    val corpusToday = LocalDate.of(2026, 6, 8)
    val corpusUrl   = Cinema1Client.screeningsUrl(CinemaId, corpusToday.atStartOfDay, corpusToday.plusYears(1).atStartOfDay)
    val fingerprint = RecordingHttpFetch.stableQueryFingerprint(new java.net.URI(corpusUrl).getRawQuery)
    val screeningsSource = Files.list(sourceRoot.resolve(s"api/cinema/$CinemaId"))
      .filter(p => p.getFileName.toString.startsWith("screening.")).findFirst().orElseThrow()
    copy(screeningsSource, targetRoot.resolve(s"api/cinema/$CinemaId/screening.$fingerprint"))

    println(s"Wrote Cinema1 corpus fixture under $targetRoot (screenings fingerprint $fingerprint)")
  }

  private def copy(source: Path, target: Path): Unit = {
    Files.createDirectories(target.getParent)
    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
    ()
  }
}
