package services.movies

import clients.TmdbClient
import models.{CinemaMovie, Country, KinoApollo, Movie, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

import java.time.LocalDateTime

/**
 * The German deployment was serving Polish TMDB content: Berlin listed
 * "Gwiezdne wojny: Mandalorian i Grogu" and Schwäbisch Gmünd showed the Polish
 * "Backrooms. Bez wyjścia" poster. Both rows carried a `Tmdb` slot resolved
 * before per-country enrichment language landed — unstamped, so Polish — which
 * `UnresolvedTmdbReaper` re-resolves once it sees the mismatched stamp.
 *
 * The hole this spec closes is in the re-resolve itself. Every localized field
 * fell back to the PREVIOUS slot (`.orElse(existingTmdbSlot.…)`) while the
 * `language` stamp was written unconditionally. So a re-resolve whose German
 * `fullDetails` came back partial — TMDB has no German title/overview/poster
 * for the film, or the payload was thin — kept the Polish text AND stamped it
 * `de-DE`. The row then reads as correctly-localised and the reaper never looks
 * at it again: the self-heal seals in the exact bug it exists to fix.
 *
 * Language-NEUTRAL fields (runtime, release year, cast, director, original
 * title) are unaffected by which language they were fetched in, so those still
 * carry over.
 */
class WrongLanguageSlotReresolveSpec extends AnyFlatSpec with Matchers {

  private val Title  = "The Mandalorian and Grogu"
  private val TmdbId = 1228710

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  /** A German-language client whose `de-DE` details payload is PARTIAL: no
   *  `title`, no `overview`, no `genres`, no `poster_path` — only the
   *  language-neutral fields. This is the shape that used to let Polish text
   *  survive a "successful" German re-resolve. */
  private def germanTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      "/search/movie" ->
        s"""{"results":[
           |{"id":$TmdbId,"title":"","original_title":"The Mandalorian and Grogu",
           | "release_date":"2026-05-22","popularity":88.0}
           |]}""".stripMargin,
      s"/movie/$TmdbId/external_ids" -> s"""{"id":$TmdbId,"imdb_id":"tt13622970"}""",
      s"/movie/$TmdbId/images"       -> """{"posters":[]}""",
      // The `language=de-DE` details call — deliberately missing every
      // localized field. `/images` is stubbed empty too, so no poster arrives.
      s"/movie/$TmdbId" ->
        s"""{"id":$TmdbId,"title":"","overview":"","original_title":"The Mandalorian and Grogu",
           | "release_date":"2026-05-22","runtime":121,"genres":[],"production_countries":[],
           | "credits":{"crew":[{"job":"Director","name":"Jon Favreau"}],"cast":[]}}""".stripMargin
    )),
    apiKey   = Some("stub"),
    language = Country.Germany.language
  )

  /** The live shape: an unstamped (= legacy Polish) `Tmdb` slot holding Polish
   *  title, poster and genres, plus the German cinema's own listing. */
  private def polishSlotRow(): MovieRecord = MovieRecord(
    tmdbId = Some(TmdbId),
    data = Map[Source, SourceData](
      Tmdb -> SourceData(
        title         = Some("Gwiezdne wojny: Mandalorian i Grogu"),
        originalTitle = Some("The Mandalorian and Grogu"),
        synopsis      = Some("Mandalorianin i Grogu wyruszają w podróż."),
        genres        = Seq("Akcja", "Przygodowy", "Sci-Fi"),
        posterUrl     = Some("https://image.tmdb.org/t/p/w500/cFNZ7lAjikPUHkXZzJOLMzGFeDT.jpg"),
        releaseYear   = Some(2026),
        language      = None // pre-fix resolve: unstamped, i.e. pl-PL
      ),
      KinoApollo -> SourceData(title = Some(Title), releaseYear = Some(2026))
    ))

  private def wire(): (CaffeineMovieCache, MovieService) = {
    val repository = new InMemoryMovieRepository(Seq((Title, Some(2026), polishSlotRow())))
    val cache      = new CaffeineMovieCache(repository, new InProcessEventBus())
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(Title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    (cache, new MovieService(cache, new InProcessEventBus(), germanTmdb()))
  }

  private def resolvedTmdbSlot(cache: CaffeineMovieCache): SourceData =
    cache.get(cache.keyOf(Title, Some(2026)))
      .flatMap(_.data.get(Tmdb))
      .getOrElse(fail("no Tmdb slot on the row after re-resolve"))

  "a German re-resolve of a Polish-frozen slot" should
    "drop the Polish title, synopsis, genres and poster rather than carrying them over" in {
    val (cache, service) = wire()

    service.resolveTmdbOnce(Title, Some(2026), originalTitle = None, director = None, force = true)

    val slot = resolvedTmdbSlot(cache)
    slot.title     should not be Some("Gwiezdne wojny: Mandalorian i Grogu")
    slot.synopsis  shouldBe empty
    slot.genres    shouldBe empty
    slot.posterUrl should not be Some("https://image.tmdb.org/t/p/w500/cFNZ7lAjikPUHkXZzJOLMzGFeDT.jpg")
  }

  it should "keep the language-neutral fields the details call did return" in {
    val (cache, service) = wire()

    service.resolveTmdbOnce(Title, Some(2026), originalTitle = None, director = None, force = true)

    val slot = resolvedTmdbSlot(cache)
    slot.originalTitle  shouldBe Some("The Mandalorian and Grogu")
    slot.runtimeMinutes shouldBe Some(121)
    slot.director       shouldBe Seq("Jon Favreau")
  }

  it should "stamp the slot with the deployment language so the reaper stops re-sweeping it" in {
    val (cache, service) = wire()

    service.resolveTmdbOnce(Title, Some(2026), originalTitle = None, director = None, force = true)

    resolvedTmdbSlot(cache).fetchedLanguageTag shouldBe Country.Germany.language.toLanguageTag
  }
}
