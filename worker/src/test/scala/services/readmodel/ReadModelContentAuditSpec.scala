package services.readmodel

import models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.InMemoryMovieRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import scala.util.Try

/**
 * The content audit reads the STORE and compares it with a fresh projection of the source row —
 * the one comparison no id-level check and not even the projector's rolling content check makes,
 * since that one compares the source with the projector's own memo of what it wrote.
 *
 * Both historical ways a card went wrong-but-present are reproduced here, and the audit must see
 * each: the lost change event (Troy / 2046 / Glastonbury, 2026-08-29..09-08) and the swallowed
 * read-model write (fixed by dfaba5ba8, "Throw a failed read-model write instead of swallowing
 * it") — the second is the one the rolling content check is blind to.
 */
class ReadModelContentAuditSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private def record(rating: Double, showtimes: Seq[Showtime]): MovieRecord =
    MovieRecord(imdbRating = Some(rating), tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"), showtimes = showtimes)))

  private def cardOf(repository: InMemoryMovieRepository): String =
    ReadModelProjection.filmIds(repository.findAll().find(_.title == "Foo").get, titleNormalizer).loneElementOf

  extension (ids: Seq[String]) private def loneElementOf: String = { ids should have size 1; ids.head }

  private def audit(card: String, repository: InMemoryMovieRepository, rm: ReadModelReader): Option[Seq[String]] =
    ReadModelContentAudit.differences(card, repository, rm)

  "a card projected from its current row" should "match, whatever share card the projector gave it" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, rm, rm)
    repository.upsert("Foo", Some(2024), record(8.0, Seq(at("2026-06-12T20:00"))))
    projector.onMovieUpsert(repository.findAll().head)
    val card = cardOf(repository)
    audit(card, repository, rm) shouldBe Some(Nil)

    // shareCard / shareCardPending are the projector's, filled from the disk: not a difference.
    rm.upsertMovie(rm.findAllMovies().head.copy(shareCard = Some("x.jpg?v=1"), shareCardPending = true))
    audit(card, repository, rm) shouldBe Some(Nil)
    projector.stop()
  }

  "a lost change event" should "leave a stale card the audit names field by field, until the content check repairs it" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val first      = new ReadModelProjector(repository, rm, rm)
    repository.upsert("Foo", Some(2024), record(8.0, Seq(at("2026-06-12T20:00"))))
    first.onMovieUpsert(repository.findAll().head)
    first.stop()
    // The source moves and no event is delivered; the stream then carries on with OTHER films —
    // the production shape (see ReadModelProjectorSpec), beyond the catch-up and the heal.
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(9.9, Seq(at("2026-07-20T18:00"))))
    repository.upsert("Bar", Some(2024), record(6.0, Seq(at("2026-06-14T20:00"))))
    val checker = new ReadModelProjector(repository, rm, rm)
    checker.start()

    val card = cardOf(repository)
    val found = audit(card, repository, rm).get
    found should contain ("ratings")
    found.exists(_.endsWith(".showtimes")) shouldBe true

    (1 to 48).foreach(_ => checker.pruneOrphans())   // the rolling content check, whole corpus
    audit(card, repository, rm) shouldBe Some(Nil)
    checker.stop()
  }

  // THE CASE ONLY A READ OF THE STORE CAN SEE. Before dfaba5ba8 a failed read-model write was
  // swallowed: the projector remembered a document the store never took, and every later
  // projection — the rolling content check's included — compared the source with that memo, found
  // them equal, and wrote nothing. `swallow` below is that writer; `throwing` is today's.
  private class DroppingWriter(store: InMemoryReadModelRepository, throwing: Boolean) extends ReadModelWriter {
    @volatile var dropping = false
    def enabled: Boolean = true
    def upsertMovie(m: ResolvedMovie): Unit =
      if (!dropping) store.upsertMovie(m) else if (throwing) throw new RuntimeException("write failed")
    def deleteMovie(id: String): Unit = store.deleteMovie(id)
    def upsertScreening(s: CityScreening): Unit =
      if (!dropping) store.upsertScreening(s) else if (throwing) throw new RuntimeException("write failed")
    def deleteScreening(id: String): Unit = store.deleteScreening(id)
    def close(): Unit = ()
  }

  private def failedWriteThenSweeps(throwing: Boolean): Option[Seq[String]] = {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val writer     = new DroppingWriter(rm, throwing)
    val projector  = new ReadModelProjector(repository, writer, rm)
    repository.upsert("Foo", Some(2024), record(8.0, Seq(at("2026-06-12T20:00"))))
    projector.onMovieUpsert(repository.findAll().head)
    repository.upsert("Foo", Some(2024), record(9.9, Seq(at("2026-07-20T18:00"))))
    writer.dropping = true
    Try(projector.onMovieUpsert(repository.findAll().find(_.title == "Foo").get))   // the write fails
    writer.dropping = false
    (1 to 48).foreach(_ => projector.pruneOrphans())                                // a whole day of content checks
    val found = audit(cardOf(repository), repository, rm)
    projector.stop()
    found
  }

  "a read-model write that failed" should "be repaired by the content check once the failure is thrown (today)" in {
    failedWriteThenSweeps(throwing = true) shouldBe Some(Nil)
  }

  it should "be caught by the audit when it was swallowed (before dfaba5ba8) — the content check cannot see it" in {
    val found = failedWriteThenSweeps(throwing = false).get
    found should contain ("ratings")
    found.exists(_.endsWith(".showtimes")) shouldBe true
  }

  "a card whose row no longer projects it" should "not be judged — the prune and the heal own that" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    audit("gone|2024", repository, rm) shouldBe None
  }

  "a card whose stored read failed" should "not be judged — a failed read is not an empty card" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    new ReadModelProjector(repository, rm, rm).onMovieUpsert({
      repository.upsert("Foo", Some(2024), record(8.0, Seq(at("2026-06-12T20:00")))); repository.findAll().head })
    val blind = new InMemoryReadModelRepository { override def findCard(id: String): Option[StoredCard] = None }
    audit(cardOf(repository), repository, blind) shouldBe None
  }

  "the comparison" should "name a missing and an unexpected screenings row" in {
    val movie = ResolvedMovie(_id = "f1", title = "Foo", originalTitle = None, posterUrl = None, fallbackPosterUrls = Nil,
      runtimeMinutes = None, releaseYear = Some(2024), genres = Nil, countries = Nil, directors = Nil, cast = Nil,
      synopsis = None, trailerUrls = Nil, ratings = ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0)
    val row   = CityScreening("f1|poznan|Muza", "f1", "poznan", "Muza", None, Seq(at("2026-06-12T20:00")))
    val other = row.copy(_id = "f1|krakow|Kijow", city = "krakow", cinema = "Kijow")
    ReadModelContentAudit.differences((movie, Seq(row)), (movie, Seq(other))) shouldBe
      Seq("screenings[f1|poznan|Muza] missing", "screenings[f1|krakow|Kijow] unexpected")
    ReadModelContentAudit.differences((movie, Seq(row)), (movie.copy(title = "Bar"), Seq(row.copy(filmUrl = Some("u"))))) shouldBe
      Seq("title", "screenings[f1|poznan|Muza].filmUrl")
  }
}
