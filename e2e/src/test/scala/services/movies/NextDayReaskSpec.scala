package services.movies


import tools.SpecClock.given

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.RatingTasks

/** The next-day leg's "why was this unchanged film rewritten" lines: which enrichment's answer moved, and which of its
 *  inputs changed on day one after its last ask with nothing asking again. */
class NextDayReaskSpec extends AnyFlatSpec with Matchers {

  private val filmKey   = CacheKey("Cienie zapomnianych przodków", Some(1965), titleNormalizer)
  private val id    = FilmId("cienie")
  private val asked = MovieRecord(tmdbId = Some(26782), imdbId = Some("tt0058642"),
    data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Тіні забутих предків"), director = Seq("Sergei Parajanov"))))
  private val credited = asked.copy(data = asked.data +
    (models.Kinoteka -> SourceData(title = Some("Cienie zapomnianych przodków"), director = Seq("Siergiej Paradżanow"))))
  private val found = credited.copy(filmwebUrl = Some("https://www.filmweb.pl/film/Cienie+zapomnianych+przodk%C3%B3w-1964-1"),
    filmwebRating = Some(7.4))

  // Verify scrape recording #23: Filmweb was asked under TMDB's director spelling and missed; a cinema's "Siergiej
  // Paradżanow" reached the film in a later projection that re-asked nothing, and the next day's pass took the page.
  "the next-day diagnosis" should "name the enrichment whose answer moved and the day-one input change no re-ask followed" in {
    val recorder  = new NextDayReask.Recorder
    val freshness = recorder.asking(new InMemoryFreshnessStore)
    recorder.resident(filmKey, Some(StoredMovieRecord(filmKey.cleanTitle, filmKey.year, asked, id)))
    freshness.markFresh(RatingTasks.dedupKey(FreshnessKind.FilmwebRating, filmKey, asked.tmdbId), FreshnessKind.FilmwebRating, java.time.Instant.EPOCH)
    recorder.resident(filmKey, Some(StoredMovieRecord(filmKey.cleanTitle, filmKey.year, credited, id)))
    val lines = NextDayReask.explain(credited, found, recorder.dayOne(id, recorder.mark()))
    lines shouldBe Seq(
      "FilmwebRating moved (filmwebUrl None → Some(https://www.filmweb.pl/film/Cienie+zapomnianych+przodk%C3%B3w-1964-1), " +
        "filmwebRating None → Some(7.4)): its last day-one ask was #1, and its inputs changed after it: " +
        "#2 cinema directors: +'Siergiej Paradżanow' (slot Kinoteka) — no re-ask followed: check MergeRetrigger's case " +
        "for the path that made that write")
  }

  it should "say an answer moved on unchanged inputs when the last ask followed every change" in {
    val recorder  = new NextDayReask.Recorder
    val freshness = recorder.asking(new InMemoryFreshnessStore)
    recorder.resident(filmKey, Some(StoredMovieRecord(filmKey.cleanTitle, filmKey.year, asked, id)))
    recorder.resident(filmKey, Some(StoredMovieRecord(filmKey.cleanTitle, filmKey.year, credited, id)))
    freshness.markFresh(RatingTasks.dedupKey(FreshnessKind.FilmwebRating, filmKey, asked.tmdbId), FreshnessKind.FilmwebRating, java.time.Instant.EPOCH)
    NextDayReask.explain(credited, found, recorder.dayOne(id, recorder.mark())) shouldBe Seq(
      "FilmwebRating moved (filmwebUrl None → Some(https://www.filmweb.pl/film/Cienie+zapomnianych+przodk%C3%B3w-1964-1), " +
        "filmwebRating None → Some(7.4)): its last day-one ask was #2, and no input of it changed after that: " +
        "it answered otherwise on unchanged inputs")
  }

  it should "name a field no re-askable enrichment writes as the identity take's" in {
    NextDayReask.explain(asked, asked.copy(tmdbId = Some(1)), NextDayReask.DayOne(Nil, Map.empty)) shouldBe
      Seq("tmdbId moved: no re-askable enrichment writes it — the identity take (see day-one lag)")
  }
}
