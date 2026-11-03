package services.movies

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Cinema, Movie, MovieRecord, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.nio.file.Files
import java.time.{LocalDate, LocalDateTime}

class ReplayCorpusFileSpec extends AnyFlatSpec with Matchers {

  private val at = LocalDateTime.of(2026, 6, 8, 18, 30)
  // A data-driven venue (reference equality) and a case-object one.
  private val venues: Seq[Cinema] =
    Cinema.all.filterNot(_.isInstanceOf[Product]).take(2) ++ Cinema.all.filter(_.isInstanceOf[Product]).take(1)
  private val awaiting = Showtime.stored(at, null, "/book/1", Some("1"), List("2D"))
  private val prefixed = Showtime.stored(at.plusHours(2), "https://example.com", "/book/2", None, Nil)
  private val whole    = Showtime(at.plusHours(4), Some("https://example.com/book/3"))

  private def row(title: String): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = None, synopsis = None, cast = Seq("A"), director = Nil,
      cinemaFilmUrls = venues.map(_ -> s"https://example.com/$title"),
      showings = Seq(LocalDate.of(2026, 6, 8) -> venues.map(CinemaShowtimes(_, Seq(awaiting, prefixed, whole)))),
      resolved = TestReadModel.resolved(title, Some(2026), MovieRecord()), slug = Some(title),
      asOf = LocalDate.of(2026, 6, 8))

  private val corpus = ReplayCorpus(
    records = Nil,
    screenings = Map("film" -> Map("slot" -> Seq(awaiting, prefixed, whole))),
    rows = Seq(row("Alpha"), row("Beta")))

  "an order pass's corpus file" should "read back equal to the corpus written, each way" in {
    venues.exists(!_.isInstanceOf[Product]) shouldBe true
    val file = Files.createTempDirectory("replay-corpus").resolve("pass0.bin.gz")
    ReplayCorpusFile.writeVerified(corpus, file)
    val copy = ReplayCorpusFile.read(file)
    copy shouldBe corpus
    corpus shouldBe copy
  }

  it should "read venues back as the roster's own instances and the awaiting prefix as this JVM's sentinel" in {
    val file = Files.createTempDirectory("replay-corpus").resolve("pass1.bin.gz")
    ReplayCorpusFile.write(corpus, file)
    val copy = ReplayCorpusFile.read(file)
    copy.rows.head.cinemaFilmUrls.map(_._1).zip(venues).foreach { case (a, b) => (a eq b) shouldBe true }
    val back = copy.screenings("film")("slot")
    back.head.awaitsRowPrefix shouldBe true
    back.head.bookingUrl shouldBe None
    back(1).bookingUrl shouldBe Some("https://example.com/book/2")
  }

  it should "tell a corpus that differs from the one written apart, as the in-row comparison would" in {
    val file = Files.createTempDirectory("replay-corpus").resolve("pass2.bin.gz")
    ReplayCorpusFile.write(corpus, file)
    ReplayCorpusFile.read(file) should not be corpus.copy(rows = corpus.rows.reverse)
  }

  it should "name every venue a corpus can hold by a key no other venue shares" in {
    ReplayCorpusFile.ambiguousKeys shouldBe empty
    // ...and one the reading JVM computes alike: no identity hash in it (run 41462794907's DE/US comparisons).
    (Cinema.all ++ models.CinemaNetworks.all).map(ReplayCorpusFile.keyOf).filter(_.matches(".*@[0-9a-f]+$")) shouldBe empty
  }
}
