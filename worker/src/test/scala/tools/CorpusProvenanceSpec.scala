package tools

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{ArchivedScrape, SuccessfulScrape}

import java.nio.file.Files
import java.time.{Instant, LocalDateTime}

/**
 * The corpus diff a convergence leg prints, on the shape of the change that turned the
 * UK leg red in run 35948292875: the day's corpus lost the Mockingjay Part 1 listings
 * that carried an original title, with no code change behind it.
 */
class CorpusProvenanceSpec extends AnyFlatSpec with Matchers {

  private def listing(cinema: Cinema, title: String, original: Option[String], showtimes: Int) = CinemaMovie(
    movie     = Movie(title, Some(123), None, Seq.empty, Seq.empty, original, None),
    cinema    = cinema,
    posterUrl = None,
    filmUrl   = None,
    synopsis  = None,
    cast      = Seq.empty,
    director  = Seq("Francis Lawrence"),
    showtimes = (1 to showtimes).map(h => Showtime(LocalDateTime.parse("2026-09-24T10:00").plusHours(h), bookingUrl = None)))

  private def venue(cinema: Cinema, films: CinemaMovie*) =
    ArchivedScrape(cinema, Some("London"), Some(SuccessfulScrape(Instant.parse("2026-09-23T01:00:00Z"),
      listingComplete = true, films = films)), None)

  private val (odeon, savoy, arc) = (Cinema.all(0), Cinema.all(1), Cinema.all(2))

  private val green = Seq(
    venue(odeon, listing(odeon, "The Hunger Games: Mockingjay - Part 1 (2014)",
                         Some("The Hunger Games: Mockingjay - Part 1"), 4)),
    venue(savoy, listing(savoy, "The Hunger Games: Mockingjay - Part 1", None, 1)))
  private val today = Seq(
    venue(savoy, listing(savoy, "The Hunger Games: Mockingjay - Part 1", None, 1)),
    venue(arc,   listing(arc,   "The Hunger Games: Mockingjay - Part 1 (2026)", None, 3)))

  "a corpus diff" should "name the venues, the films and the field that disappeared" in {
    val diff = CorpusDiff.of(green, today)

    diff.venuesAdded shouldBe Seq(arc.displayName)
    diff.venuesRemoved shouldBe Seq(odeon.displayName)
    diff.filmDeltas shouldBe Seq(
      ("The Hunger Games: Mockingjay - Part 1 (2014)", 4, 0),
      ("The Hunger Games: Mockingjay - Part 1 (2026)", 0, 3))
    diff.coverageDrops.map(_.field) shouldBe Seq("originalTitle")
    diff.summary should include ("originalTitle 50.0% (1/2) → 0.0% (0/2)")
    diff.identical shouldBe false
  }

  it should "call two identical corpora identical" in {
    CorpusDiff.of(today, today).identical shouldBe true
  }

  private def env(pairs: (String, String)*): settings.ProcessConfiguration = new settings.ProcessConfiguration(Env.of(pairs*))

  "the provenance verdict" should "say CHANGED, with the diff, when the green leg replayed another corpus" in {
    val dir = Files.createTempDirectory("green-corpus")
    Files.write(dir.resolve("cinema-scrapes-uk.json.gz"), gzip(CorpusFixture.render(green)))

    val provenance = CorpusProvenance.of("uk", today, env(
      CorpusProvenance.RunEnv -> "35943410096", CorpusProvenance.RecordedAtEnv -> "2026-09-24T01:32:59Z",
      CorpusProvenance.GreenRunEnv -> "35806936931", CorpusProvenance.GreenRecordedAtEnv -> "2026-09-23T01:35:45Z",
      CorpusProvenance.GreenDirEnv -> dir.toString))

    provenance.verdict should include ("Corpus CHANGED since the last green leg")
    provenance.verdict should include ("35806936931")
    provenance.verdict should include ("originalTitle")
    provenance.markdown("uk") should include (odeon.displayName)
  }

  it should "say UNCHANGED when the green leg replayed the same recording" in {
    CorpusProvenance.of("uk", today, env(
      CorpusProvenance.RunEnv -> "35943410096", CorpusProvenance.GreenRunEnv -> "35943410096"))
      .verdict should include ("UNCHANGED")
  }

  it should "admit it cannot tell when nothing was recorded" in {
    CorpusProvenance.of("uk", today, env()).verdict should include ("unknown")
    CorpusProvenance.of("uk", today, env(CorpusProvenance.RunEnv -> "1")).verdict should include ("cannot be ruled out")
  }

  private def gzip(s: String): Array[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream()
    val out   = new java.util.zip.GZIPOutputStream(bytes)
    try out.write(s.getBytes("UTF-8")) finally out.close()
    bytes.toByteArray
  }
}
