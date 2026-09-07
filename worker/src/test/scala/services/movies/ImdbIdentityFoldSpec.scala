package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/** The settle's imdbId edge, asked at write time: TMDB holding one film under two ids
 *  shows up as two resolutions sharing an IMDb id. The settle would union them a tick
 *  later and keep the tmdbId the cinemas' runtimes corroborate; the write does the same,
 *  so there is never a pair to merge — unless the cinemas describe two films. */
class ImdbIdentityFoldSpec extends AnyFlatSpec with Matchers {

  private def cache() = new CaffeineMovieCache(new InMemoryMovieRepository, normalizer = titleNormalizer)

  private def resolved(title: String, tmdbId: Int, tmdbRuntime: Int, cinema: Cinema, cinemaRuntime: Int): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), imdbId = Some("tt0123456"), data = Map[Source, SourceData](
      Tmdb            -> SourceData(title = Some(title), originalTitle = Some(title), releaseYear = Some(2025), runtimeMinutes = Some(tmdbRuntime)),
      (cinema: Source) -> SourceData(title = Some(title), releaseYear = Some(2025), runtimeMinutes = Some(cinemaRuntime))))

  "put" should "fold a row sharing an imdbId under another tmdbId into the existing row, keeping the corroborated tmdbId" in {
    val c   = cache()
    val a   = CacheKey("Ghost", Some(2025), titleNormalizer)
    val b   = CacheKey("Ghost 2 Big Tour", Some(2025), titleNormalizer)
    c.put(a, resolved("Ghost", tmdbId = 1001, tmdbRuntime = 98, KinoMuza, cinemaRuntime = 98))
    // A second resolution of the SAME film: TMDB's duplicate entry, whose runtime the
    // venues contradict (they publish 98 min, the duplicate says 45).
    c.put(b, resolved("Ghost 2 Big Tour", tmdbId = 2002, tmdbRuntime = 45, Helios, cinemaRuntime = 98))

    val rows = c.entries
    withClue(s"rows: ${rows.map(_._1.cleanTitle)}\n")(rows should have size 1)
    val (_, merged) = rows.head
    merged.tmdbId shouldBe Some(1001)
    merged.imdbId shouldBe Some("tt0123456")
    merged.cinemaShowings.map(_._1).toSet shouldBe Set(KinoMuza, Helios)
  }

  it should "keep two rows when the cinemas describe two different films despite the shared imdbId" in {
    val c = cache()
    c.put(CacheKey("Ghost", Some(2025), titleNormalizer),
      MovieRecord(tmdbId = Some(1001), imdbId = Some("tt0123456"), data = Map[Source, SourceData](
        Tmdb -> SourceData(title = Some("Ghost"), originalTitle = Some("Ghost"), releaseYear = Some(2025), runtimeMinutes = Some(98)),
        (KinoMuza: Source) -> SourceData(title = Some("Ghost"), originalTitle = Some("Ghost"), releaseYear = Some(2025), runtimeMinutes = Some(98), director = Seq("Anna Nowak")))))
    c.put(CacheKey("Other Ghost", Some(2025), titleNormalizer),
      MovieRecord(tmdbId = Some(2002), imdbId = Some("tt0123456"), data = Map[Source, SourceData](
        Tmdb -> SourceData(title = Some("Other Ghost"), originalTitle = Some("Other Ghost"), releaseYear = Some(2025), runtimeMinutes = Some(150)),
        (Helios: Source) -> SourceData(title = Some("Other Ghost"), originalTitle = Some("Another Story"), releaseYear = Some(2025), runtimeMinutes = Some(150), director = Seq("Jan Kowalski")))))

    c.entries should have size 2
  }
}
