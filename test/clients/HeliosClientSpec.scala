package clients

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

class HeliosClientSpec extends AnyFlatSpec with Matchers {

  private val FixtureDir = "test/fixtures/helios"

  private def readFixture(name: String): String =
    new String(Files.readAllBytes(Paths.get(s"$FixtureDir/$name")), "UTF-8")

  private def allMovieBodies: Map[String, String] = {
    val dir = Paths.get(FixtureDir)
    Files.list(dir).iterator().asScala
      .filter(_.getFileName.toString.startsWith("movie_"))
      .map { p =>
        val movieId = p.getFileName.toString.stripPrefix("movie_").stripSuffix(".json")
        movieId -> new String(Files.readAllBytes(p), "UTF-8")
      }
      .toMap
  }

  private def allScreenBodies: Map[String, String] = {
    val dir = Paths.get(FixtureDir)
    Files.list(dir).iterator().asScala
      .filter(_.getFileName.toString.startsWith("screen_"))
      .map { p =>
        val screenId = p.getFileName.toString.stripPrefix("screen_").stripSuffix(".json")
        screenId -> new String(Files.readAllBytes(p), "UTF-8")
      }
      .toMap
  }

  // ── Smoke test ────────────────────────────────────────────────────────────

  "HeliosClient.buildCinemaMovies" should "return results from real fixture data" in {
    val result = HeliosClient.buildCinemaMovies(
      nuxtHtml       = readFixture("repertoire.html"),
      screeningsBody = readFixture("screenings.json"),
      movieBodies    = allMovieBodies,
      screenBodies   = allScreenBodies
    )
    result                    should not be empty
    result.size               should be >= 5
    result.flatMap(_.showtimes) should not be empty
  }

  it should "assign unique poster URLs — no movie steals another's poster" in {
    val result = HeliosClient.buildCinemaMovies(
      nuxtHtml       = readFixture("repertoire.html"),
      screeningsBody = readFixture("screenings.json"),
      movieBodies    = allMovieBodies,
      screenBodies   = allScreenBodies
    )

    val posterUrls = result.flatMap(_.posterUrl)
    // Every poster URL that appears should be distinct (no cross-contamination).
    posterUrls.distinct.size shouldBe posterUrls.size
  }

  // ── Collision regression ───────────────────────────────────────────────────
  //
  // Real fixture data contains "O psie, który jeździł koleją" (movieId de2de832)
  // which has exactly ONE screening — at 2026-05-14T10:00 — that it shares with
  // "Sprawiedliwość owiec" (movieId 0c138744, 18 screenings).
  //
  // The buggy code (Map keyed by LocalDateTime, pick first match) would find the
  // collision slot first for "O psie" and, because 0c138744 appears later in the
  // JSON array and overwrites de2de832 in the map, hand "O psie" the wrong poster.
  //
  // The fix (keyed by API screening UUID from the booking URL) gives a guaranteed
  // 1:1 match regardless of ordering or collision count.

  it should "assign the correct poster to a movie whose only screening shares a timeslot with another movie" in {
    val result = HeliosClient.buildCinemaMovies(
      nuxtHtml       = readFixture("repertoire.html"),
      screeningsBody = readFixture("screenings.json"),
      movieBodies    = allMovieBodies,
      screenBodies   = allScreenBodies
    )

    val oPsie = result.find(_.movie.title.startsWith("O psie"))
    oPsie                    shouldBe defined
    oPsie.get.showtimes.size shouldBe 1
    oPsie.get.posterUrl      shouldBe Some("https://movies.helios.pl/images/opsieplakat.jpg")
  }

  it should "not assign Sprawiedliwość owiec's poster to O psie" in {
    val result = HeliosClient.buildCinemaMovies(
      nuxtHtml       = readFixture("repertoire.html"),
      screeningsBody = readFixture("screenings.json"),
      movieBodies    = allMovieBodies,
      screenBodies   = allScreenBodies
    )

    val wrongPoster = "https://movies.helios.pl/images/Sprawiedliwoscowiecplakat.jpg"
    result
      .find(_.movie.title.startsWith("O psie"))
      .flatMap(_.posterUrl) should not be Some(wrongPoster)
  }
}
