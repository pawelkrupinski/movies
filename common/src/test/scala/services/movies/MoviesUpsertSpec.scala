package services.movies

import models.{MovieRecord, Multikino, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}
import scala.util.{Failure, Success, Try}

/**
 * The `movies` write decision `MongoMovieRepository.upsert` makes, pinned case by case.
 *
 * The one that matters most is the last: a read that FAILED must write. `stored` folding a
 * failure into "absent" is exactly how a failed read once became "the row is empty, merge
 * onto nothing" and wiped the ratings the refreshers own — here the failure has to reach
 * the decision as a failure, and the decision has to answer "write".
 */
class MoviesUpsertSpec extends AnyFlatSpec with Matchers {

  private val id      = "zaplatani|2010"
  private val now     = Instant.parse("2026-09-06T10:00:00Z")
  private val earlier = now.minusSeconds(3600)
  private val showing = Showtime(LocalDateTime.of(2026, 9, 7, 18, 0), Some("https://book/18"))
  private val record  = MovieRecord(
    tmdbId = Some(38757), imdbRating = Some(7.7),
    data   = Map[Source, SourceData](Multikino -> SourceData(title = Some("Zaplatani"), showtimes = Seq(showing))))

  private val keepSlots: Map[Source, SourceData] => Map[Source, SourceData] = identity

  private def plan(stored: Try[Option[StoredMovieDto]], slotsLanded: Boolean = false) =
    MoviesUpsert.plan(id, record, record.data, slotsLanded, keepSlots, stored, now)

  "the movies write" should "go ahead when no document is stored" in {
    plan(Success(None)).unchanged shouldBe false
  }

  it should "be skipped when the stored document already equals it, timestamps aside" in {
    // Stamped an hour ago, and with the slot marker `updateIfPresent` leaves behind — neither
    // may count as a difference, or the guard never fires on a film whose slots were patched.
    val stored = StoredMovieDto.fromDomain(id, record, earlier).copy(slotsUpdatedAt = Some(earlier))
    plan(Success(Some(stored))).unchanged shouldBe true
  }

  it should "go ahead when the stored document differs" in {
    val stored = StoredMovieDto.fromDomain(id, record.copy(imdbRating = Some(7.4)), earlier)
    plan(Success(Some(stored))).unchanged shouldBe false
  }

  it should "go ahead when the stored document could not be READ — a failed read is not a match" in {
    plan(Failure(new RuntimeException("connection timed out"))).unchanged shouldBe false
  }

  "the document it writes" should "carry no slots once they have landed in movie_slots" in {
    plan(Success(None), slotsLanded = true).document shouldBe
      StoredMovieDto.fromDomain(id, record.copy(data = Map.empty), now)
  }

  it should "carry the slots in the repository's storage shape when they have not landed" in {
    val stripped: Map[Source, SourceData] => Map[Source, SourceData] = ScreeningsSplit.stripShowtimes
    MoviesUpsert.plan(id, record, record.data, slotsLanded = false, stripped, Success(None), now).document shouldBe
      StoredMovieDto.fromDomain(id, record.copy(data = stripped(record.data)), now)
    plan(Success(None)).document shouldBe StoredMovieDto.fromDomain(id, record, now)
  }
}
