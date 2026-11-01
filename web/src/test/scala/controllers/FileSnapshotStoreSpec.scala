package controllers

import tools.SpecClock.given

import models.{CinemaCityWroclavia, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.time.{Instant, LocalDateTime}

/** The /debug snapshots survive a restart on disk — every kind the pages keep must
 *  round-trip, and a file that doesn't decode must read as "nothing stored", never
 *  as an error page. */
class FileSnapshotStoreSpec extends AnyFlatSpec with Matchers {

  private val at = Instant.parse("2026-10-03T12:00:00Z")

  private def store() = new FileSnapshotStore(Files.createTempDirectory("debug-snapshots"))

  private val record = MovieRecord(tmdbId = Some(7), data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"),
    showtimes = Seq(Showtime(LocalDateTime.parse("2026-10-05T18:30"), bookingUrl = Some("https://book.example/b"))))))

  "a stored corpus listing" should "round-trip" in {
    val s = store()
    val listing = CorpusListing.read(new services.movies.InMemoryMovieRepository(Seq(("Belle", Some(2021), record)),
      normalizer = services.movies.SingleCountryNormalizer.titleNormalizer))
    s.save("/debug listing pl", DebugSnapshot(listing, Some(at.minusSeconds(5)), Some(at)))
    s.load[CorpusListing]("/debug listing pl") shouldBe Some(DebugSnapshot(listing, Some(at.minusSeconds(5)), Some(at)))
  }

  "a stored read-model dump" should "round-trip" in {
    val s    = store()
    val dump = ReadModelDump.of(services.readmodel.TestReadModel.fromRecords(Seq(("Belle", Some(2021), record))))
    dump.movies should not be empty
    s.save("/debug/readmodel pl", DebugSnapshot(dump, None, Some(at)))
    s.load[ReadModelDump]("/debug/readmodel pl").map(_.value) shouldBe Some(dump)
  }

  "stored cadence records" should "round-trip" in {
    val s       = store()
    val records = Seq("imdb:7" -> services.cadence.RatingChangeStats(3, 10, 1, at.minusSeconds(86400), at))
    s.save("/debug/cadence pl", DebugSnapshot(records, None, Some(at)))
    s.load[Seq[(String, services.cadence.RatingChangeStats)]]("/debug/cadence pl").map(_.value) shouldBe Some(records)
  }

  "a missing or unreadable file" should "read as nothing stored" in {
    val dir = Files.createTempDirectory("debug-snapshots")
    val s   = new FileSnapshotStore(dir)
    s.load[Int]("/debug listing uk") shouldBe None
    Files.writeString(dir.resolve("debug-listing-uk.ser"), "not a snapshot")
    s.load[Int]("/debug listing uk") shouldBe None
  }
}
