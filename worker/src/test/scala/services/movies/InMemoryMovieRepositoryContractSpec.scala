package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The in-memory repository must make the same WRITE DECISIONS as the Mongo one, because a
 * fake that decides differently lets a spec pass against rules production does not follow —
 * which is the most expensive kind of green this repository has.
 *
 * THE CASE THAT CAUGHT IT. `MovieRepository.upsert` re-stitches a film's showtimes out of
 * `screenings` before writing, because a record can arrive stripped for cache residency and
 * `showtimesOf` would otherwise drop showtimes that `replaceFilm` then DELETES. When that
 * re-stitch READ fails, the film looks showtime-less for a reason that has nothing to do with
 * the film, so production patches slots individually rather than issuing the full replace that
 * prunes everything it cannot see. The fake issued the full replace unconditionally: seeded
 * screenings, one upsert of a cache-shaped record, and the film's showtimes were gone.
 *
 * `MovieRepositoryIntegrationSpec` pins the same rule against real Mongo. This is the unit-level
 * twin, and it exists so the two implementations cannot drift apart again — they now share the
 * decision in [[ScreeningsSplit.applyFilm]], and this is what proves the fake calls it.
 */
class InMemoryMovieRepositoryContractSpec extends AnyFlatSpec with Matchers {

  private val when  = LocalDateTime.of(2026, 8, 1, 20, 0)
  private val times = Seq(Showtime(when, None))
  private val title = "Restitch Read Failed"
  private val year  = Some(2026)

  private def screened  = SourceData(title = Some(title), showtimes = times)
  /** The cache-resident shape: the slot is there, its showtimes are not. */
  private def stripped  = SourceData(title = Some(title), showtimes = Nil)

  private def record(slot: SourceData) =
    MovieRecord(tmdbId = Some(7), data = Map[Source, SourceData](Multikino -> slot))

  private def showtimesIn(store: InMemoryScreeningsRepository): Int =
    store.findAll().values.flatMap(_.values).map(_.size).sum

  "InMemoryMovieRepository.upsert" should "keep a film's screenings when the re-stitch read failed" in {
    val store = new InMemoryScreeningsRepository
    // Seed through the ordinary path, so the rows land under the id and slot key production uses.
    new InMemoryMovieRepository(screenings = Some(store)).upsert(title, year, record(screened))
    showtimesIn(store) shouldBe 1

    // Now the same film written again by a repository whose screenings READS all fail, from a
    // record that carries the slot without its showtimes. Nothing about the film has changed;
    // the only thing that changed is that we could not read what is already there.
    val blind = new InMemoryMovieRepository(screenings = Some(new UnreadableScreeningsRepository(store)))
    blind.upsert(title, year, record(stripped))

    // The showtimes must survive. Before the fake shared production's rule it issued
    // `replaceFilm(id, Map.empty)` here and this read 0.
    showtimesIn(store) shouldBe 1
  }

  it should "still write a film's screenings when the re-stitch read succeeded" in {
    val store = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(store))
    repository.upsert(title, year, record(screened))

    // The guard must not become "never write": a real change, read cleanly, still lands.
    val later = Seq(Showtime(when.plusHours(3), None))
    repository.upsert(title, year, record(SourceData(title = Some(title), showtimes = later)))

    store.findAll().values.flatMap(_.values).flatten.toSeq shouldBe later
  }

  // The bounded catch-up for a silent change stream reads "every row written after this
  // instant" — off `movies.updatedAt` in Mongo, and off the same stamp here, kept by the
  // fake on every write so a spec about the catch-up cannot pass against rows it would
  // not find in production.
  "InMemoryMovieRepository.foreachRecordUpdatedSince" should "yield only the rows written after the instant, stitched" in {
    val clock  = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val t0     = clock.instant()
    val store  = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(store), clock = clock)
    repository.upsert("Written First", year, record(screened))
    clock.advanceSeconds(60)
    repository.upsert("Written Second", year, record(screened))

    def updatedSince(since: java.time.Instant): Seq[StoredMovieRecord] = {
      val rows = Seq.newBuilder[StoredMovieRecord]
      repository.foreachRecordUpdatedSince(since)(rows += _) shouldBe true
      rows.result()
    }
    // By id: a read row's TITLE is re-derived from its cinema slot (the same for both here).
    def idOf(t: String) = FilmId(StoredMovieRecord.keyFor(t, year, repository.normalizer))

    updatedSince(t0).map(_.id)                shouldBe Seq(idOf("Written Second")) // strictly after, like Mongo's $gt
    updatedSince(t0.plusSeconds(60))          shouldBe empty
    updatedSince(t0.minusSeconds(1)).map(_.id) should contain theSameElementsAs Seq(idOf("Written First"), idOf("Written Second"))
    // Stitched like every other read: the showtimes live in `screenings`, and a catch-up that
    // re-projected a showtime-less row would wipe the film's screenings off the site.
    updatedSince(t0).head.record.data.values.flatMap(_.showtimes) shouldBe times

    // An out-of-band write — the store changed, no change event — is exactly what the
    // catch-up exists to find.
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Written First", year, record(screened))
    updatedSince(t0.plusSeconds(60)).map(_.id) shouldBe Seq(idOf("Written First"))
  }
}
