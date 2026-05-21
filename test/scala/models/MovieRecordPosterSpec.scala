package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.fallbackPosterUrl` — TMDB poster offered as a
 * `data-fallback` swap-target so the view can recover from broken
 * cinema-side poster URLs (e.g. Cinema City returning a posterLink
 * to a file they haven't actually uploaded). The accessor is `None`
 * when there's no TMDB slot, and `None` when TMDB's poster is the
 * same as the primary (no point shipping the URL twice).
 */
class MovieRecordPosterSpec extends AnyFlatSpec with Matchers {

  "fallbackPosterUrl" should "return the TMDB poster when it differs from the primary cinema poster" in {
    // Mirrors the Drishyam-3 scenario: Cinema City supplied a broken
    // poster URL, TMDB resolved the film and stored a working one.
    val rec = MovieRecord(
      tmdbId = Some(847742),
      data = Map[Source, SourceData](
        CinemaCityKinepolis -> SourceData(posterUrl = Some("https://www.cinema-city.pl/.../8215S2R.jpg")),
        Tmdb                -> SourceData(posterUrl = Some("https://media.themoviedb.org/t/p/w500/x.jpg"))
      )
    )
    rec.posterUrl         shouldBe Some("https://www.cinema-city.pl/.../8215S2R.jpg")
    rec.fallbackPosterUrl shouldBe Some("https://media.themoviedb.org/t/p/w500/x.jpg")
  }

  it should "be None when the TMDB slot has no poster" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(posterUrl = Some("https://www.multikino.pl/x.jpg")),
        Tmdb      -> SourceData(posterUrl = None)
      )
    )
    rec.fallbackPosterUrl shouldBe None
  }

  // When cinema and TMDB happen to surface the same URL (rare but
  // possible during testing or after a manual backfill), no point
  // emitting the same URL twice on the <img>.
  it should "be None when TMDB's poster matches the primary" in {
    val u = "https://image.tmdb.org/t/p/w500/x.jpg"
    val rec = MovieRecord(
      data = Map[Source, SourceData](Tmdb -> SourceData(posterUrl = Some(u)))
    )
    rec.posterUrl         shouldBe Some(u)
    rec.fallbackPosterUrl shouldBe None
  }

  // Films with no TMDB enrichment yet — no fallback to offer; the
  // <img> renders without `data-fallback` and `onerror` falls
  // straight through to the .no-poster placeholder.
  it should "be None when there's no TMDB slot at all" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Helios -> SourceData(posterUrl = Some("https://img.helios.pl/x.jpg"))
      )
    )
    rec.fallbackPosterUrl shouldBe None
  }
}
