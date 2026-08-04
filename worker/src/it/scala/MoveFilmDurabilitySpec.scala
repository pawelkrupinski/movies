package services.movies

import models.{Multikino, Showtime}
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * `moveFilm` carries a film's side rows to a new id and then deletes the old ones. If the
 * COPY silently fails and the delete still runs, the showtimes are gone — and a Mongo
 * transaction would not save it, because nothing throws and there is nothing to roll back:
 * `ScreeningsRepository.replaceFilm` returns `Unit` and swallows its own errors.
 *
 * So the property that matters is VERIFY-THEN-DELETE — the old rows may only go once the
 * new id demonstrably holds them. Driven against the REAL `MongoMovieRepository` with a
 * screenings store that accepts no writes.
 */
class MoveFilmDurabilitySpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val when = java.time.LocalDateTime.now().plusDays(2).withHour(19).withMinute(0).withSecond(0).withNano(0)

  it should "keep the old rows when the copy to the new id did not land" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    try {
      val screenings = new UnwritableScreeningsRepository
      screenings.seed("moveprobe|", Map(Multikino.displayName -> Seq(Showtime(when, None))))
      screenings.findForFilm("moveprobe|") should not be empty

      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), normalizer = titleNormalizer)
      repository.moveFilm("moveprobe|", "moveprobe|2026")

      withClue("the copy never landed, so the old rows must still be there: ")(
        screenings.findForFilm("moveprobe|") should not be empty)
    } finally client.close()
  }
}
