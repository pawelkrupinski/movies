package scripts

import models.{Cinema, CinemaShowing, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository, ListedShowtimes, TitleNormalizer}

import java.time.LocalDateTime

class AddMissingMovieSlotsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(models.Country.Poland)
  private val cinema: Cinema = Cinema.all.head
  private val shown          = CinemaShowing(cinema, "anora")
  private val gone           = CinemaShowing(cinema, "anora (dubbing)")
  private def at(hour: Int)  = Showtime(LocalDateTime.of(2026, 10, 3, hour, 0), None)

  /** A film the way prod still holds ~230 of them: both slots embedded in `movies`, a showtimes row
   *  for one of them, and no `movie_slots` row at all. */
  private def legacy() = {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    val written    = new InMemoryMovieRepository(screenings = Some(screenings), normalizer = normalizer)
    written.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1064213), data = Map[Source, SourceData](
      shown -> SourceData(title = Some("Anora"), showtimes = Seq(at(20))),
      gone  -> SourceData(title = Some("Anora"), showtimes = Seq.empty))))
    val id = written.findAll().head.id.value
    screenings.upsertSlot(id, shown.displayName, ListedShowtimes(Seq(at(20)), None))
    (written, slots, screenings, id)
  }

  /** `MovieRepository.readVenues`' own condition: every showtimes row at the venue has a slot row
   *  beside it — else the venue read declines and the change stream re-reads the whole film. */
  private def venueReadable(slots: InMemorySlotsRepository, screenings: InMemoryScreeningsRepository, id: String): Boolean = {
    val names = Set(cinema.displayName)
    screenings.findAtCinemasChecked(id, names).required.keySet.subsetOf(slots.findAtCinemasChecked(id, names).required.keySet)
  }

  "AddMissingMovieSlots" should "give a legacy film the slot rows its showtimes need, so its venue read stops declining" in {
    val (written, slots, screenings, id) = legacy()
    venueReadable(slots, screenings, id) shouldBe false

    val (counts, complete) = AddMissingMovieSlots.run(written, slots, screenings, apply = true)

    complete shouldBe true
    counts.repaired shouldBe 1
    counts.rows shouldBe 1
    slots.findForFilm(id).keySet shouldBe Set(shown.displayName)
    venueReadable(slots, screenings, id) shouldBe true
  }

  it should "write nothing on a dry run" in {
    val (written, slots, screenings, id) = legacy()
    AddMissingMovieSlots.run(written, slots, screenings, apply = false)._1.rows shouldBe 1
    slots.findForFilm(id) shouldBe empty
  }

  it should "never replace a row that exists, and touch nothing on a second run" in {
    val (written, slots, screenings, id) = legacy()
    val current = SourceData(title = Some("Anora — current"))
    slots.upsertSlot(id, shown.displayName, current)
    AddMissingMovieSlots.run(written, slots, screenings, apply = true)._1.rows shouldBe 0
    slots.findForFilm(id)(shown.displayName).title shouldBe Some("Anora — current")
  }
}
