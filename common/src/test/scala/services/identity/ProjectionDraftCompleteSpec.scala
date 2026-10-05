package services.identity

import models.{Cinema, CinemaShowing, Helios, KinoApollo, KinoMuza, MovieRecord, Multikino, Rialto, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, ShowtimesDigest, SingleCountryNormalizer}

import java.time.LocalDateTime

class ProjectionDraftCompleteSpec extends AnyFlatSpec with Matchers {

  /** A cast that counts how often it is read: a film's record hashed reads every slot's cast (`SourceData.hashCode`). */
  private final class CountingCast(names: String*) extends scala.collection.immutable.AbstractSeq[String] {
    var reads = 0
    def apply(i: Int): String = names(i)
    def length: Int = names.length
    def iterator: Iterator[String] = { reads += 1; names.iterator }
  }

  // A wide release's record is thousands of slots: hashing it, once per venue it differs at, was 11% of a US light tick.
  "complete" should "build a film's differing venues without hashing the film" in {
    val cast    = new CountingCast("Timothée Chalamet", "Zendaya")
    // Five venues: a set of up to four holds its elements unhashed.
    val cinemas = Seq[Cinema](Helios, KinoApollo, KinoMuza, Multikino, Rialto)
    val at      = (cinema: Cinema) => CinemaShowing.keyFor(cinema, "Diuna", SingleCountryNormalizer.titleNormalizer)
    val slot    = (hour: Int) => ShowtimesDigest.stripSlot(SourceData(title = Some("Diuna"), filmUrl = Some("https://venue/diuna"),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 10, 6, hour, 0), None))))
    val film    = ProjectedFilm(FilmId("diuna|2021"), 1L, "Diuna", Some(2021), "diuna|2021",
      MovieRecord(tmdbId = Some(438631), data = cinemas.map(c => at(c) -> slot(18)).toMap + (Tmdb -> SourceData(title = Some("Dune"), cast = cast))), Nil)
    val draft   = ProjectionDraft(Nil, Nil, Nil, FilmIdCounters.empty, Nil, Regroupings(0, 0, 0, 0, 0), Map.empty,
      venues = Map(1L -> cinemas.map(_ -> Nil).toMap), build = (cinema, _, _) => Seq(at(cinema) -> slot(20)))

    val Seq(completed) = draft.complete(Seq(film), _ => None)

    cinemas.map(c => completed.record.data(at(c))) shouldBe cinemas.map(_ => slot(20))
    cast.reads shouldBe 0
  }
}
