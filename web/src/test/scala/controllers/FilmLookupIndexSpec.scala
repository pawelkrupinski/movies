package controllers

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.{InMemoryReadModelRepository, TestReadModel, WebReadModel}

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger

/** The legacy `?title=` lookup and the slug lookup resolve through the read
 *  model's memoised indexes, not by walking the corpus per request.
 *
 *  `film(city, title)` used to scan every movie the read model holds, folding
 *  each title through `TitleText.normalize` on every miss — a per-request
 *  O(corpus) walk on the one path (an old shared link) that most needs to be
 *  cheap. The indexes are built once per read-model version, like `filmSlugs`,
 *  so a lookup is a map read. This pins both halves: the answers are the ones
 *  the scan gave, and `allMovies()` is no longer consulted to give them.
 */
class FilmLookupIndexSpec extends AnyFlatSpec with Matchers {

  /** A read model that counts corpus walks, and the films joined per lookup. */
  private class CountingReadModel(store: InMemoryReadModelRepository) extends WebReadModel(store) {
    val allMoviesCalls = new AtomicInteger(0)
    val movieCalls     = new AtomicInteger(0)
    override def allMovies(): Seq[ResolvedMovie] = { allMoviesCalls.incrementAndGet(); super.allMovies() }
    override def movie(id: String): Option[ResolvedMovie] = { movieCalls.incrementAndGet(); super.movie(id) }
  }

  private def showing(at: LocalDateTime) = Seq(Showtime(at, None, None, Nil))
  private val now = LocalDateTime.now()

  private val records = Seq(
    // Live in Poznań, and displayed with an Arabic numeral — the shape an old
    // "Rocky II" link has to keep resolving.
    ("Rocky 2", Some(1979), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Rocky 2"), releaseYear = Some(1979), showtimes = showing(now.plusHours(2)))))),
    // Known to the read model, but its run has ended — the deep-link fallback.
    ("Drugie życie", Some(2026), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Drugie życie"), releaseYear = Some(2026), showtimes = showing(now.minusDays(1)))))),
  )

  /** A warm service: one live lookup has built every memoised index, so what
   *  the counter sees afterwards is the per-request cost alone. */
  private def warmService(): (MovieControllerService, CountingReadModel) = {
    val readModel = new CountingReadModel(TestReadModel.store(records))
    readModel.reload()
    val service = new MovieControllerService(readModel)
    service.film(Poznan, "Rocky 2").map(_.movie.title) shouldBe Some("Rocky 2")
    readModel.allMoviesCalls.set(0)
    readModel.movieCalls.set(0)
    (service, readModel)
  }

  // A request for ONE film used to build the schedule of EVERY film in the city
  // and pick its own out of the list: 2,000 joins for one card, ~4 ms a request
  // on a 2,000-card city, on the page nothing caches. The index names the film;
  // only that film's screenings are joined.
  "film by slug" should "join only the addressed film, not the whole city" in {
    val (service, readModel) = warmService()
    service.filmBySlug(Poznan, "rocky-2").map(_.movie.title) shouldBe Some("Rocky 2")
    readModel.movieCalls.get() shouldBe 1
  }

  "film by title" should "join only the films the title index names, not the whole city" in {
    val (service, readModel) = warmService()
    service.film(Poznan, "Rocky 2").map(_.movie.title) shouldBe Some("Rocky 2")
    readModel.movieCalls.get() shouldBe 1
  }

  "film by title" should "resolve a roman-numeral spelling of an Arabic-numeral display title" in {
    val (service, _) = warmService()
    val found = service.film(Poznan, "Rocky II")
    found.map(_.movie.title)      shouldBe Some("Rocky 2")
    found.map(_.showings.nonEmpty) shouldBe Some(true)
  }

  it should "serve a known film with no live schedule without walking the corpus" in {
    val (service, readModel) = warmService()
    val found = service.film(Poznan, "Drugie życie")
    found.map(_.movie.title) shouldBe Some("Drugie życie")
    found.map(_.showings)    shouldBe Some(Seq.empty)
    readModel.allMoviesCalls.get() shouldBe 0
  }

  it should "still take the doubly-encoded spelling a chat app pastes" in {
    val (service, readModel) = warmService()
    service.film(Poznan, "Drugie%20%C5%BCycie").map(_.movie.title) shouldBe Some("Drugie życie")
    readModel.allMoviesCalls.get() shouldBe 0
  }

  it should "answer None for an unknown title without walking the corpus" in {
    val (service, readModel) = warmService()
    service.film(Poznan, "Nie ma takiego filmu") shouldBe None
    readModel.allMoviesCalls.get() shouldBe 0
  }

  "film by slug" should "serve a known film with no live schedule through the slug index, not a corpus walk" in {
    val (service, readModel) = warmService()
    val found = service.filmBySlug(Poznan, "drugie-zycie")
    found.map(_.movie.title) shouldBe Some("Drugie życie")
    found.map(_.showings)    shouldBe Some(Seq.empty)
    readModel.allMoviesCalls.get() shouldBe 0
  }

  it should "still answer None for a slug nothing owns" in {
    val (service, _) = warmService()
    service.filmBySlug(Poznan, "nie-ma-takiego") shouldBe None
  }
}
