package clients.kino_eva

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoEva
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoEvaClient
import tools.HttpStatusException

import java.time.LocalDateTime

/** Replays Kino Eva's (Międzyzdroje) one-page repertoire as captured on
 *  2026-09-27 over plain HTTP: seven day headers, 25 Sep – 1 Oct 2026, one of
 *  them a closed Monday ("- KINO NIECZYNNE", no table).
 *  Fixture directory: test/resources/fixtures/kino-eva/ */
class KinoEvaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoEvaClient(new FakeHttpFetch("kino-eva")).fetch()

  "KinoEvaClient" should "return every film of the week, a shouted title grouped with its normally typed twin" in {
    movies.map(_.movie.title) shouldBe Seq(
      "100 dni: Misja Zeus", "Mistyczka", "Niebo nad Normandią", "Spa weekend",
      "Spider-Man: Całkiem Nowy Dzień", "Tedi i magiczna Lampa"
    )
    movies.map(_.cinema).toSet shouldBe Set(KinoEva)
  }

  it should "anchor each row to its day header, skipping the closed day" in {
    val tedi = movies.find(_.movie.title == "Tedi i magiczna Lampa").value
    tedi.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 25, 15, 45),
      LocalDateTime.of(2026, 9, 26, 14, 0), LocalDateTime.of(2026, 9, 26, 15, 50),
      LocalDateTime.of(2026, 9, 27, 14, 0), LocalDateTime.of(2026, 9, 27, 15, 50),
      LocalDateTime.of(2026, 9, 29, 15, 50),
      LocalDateTime.of(2026, 9, 30, 15, 50),
      LocalDateTime.of(2026, 10, 1, 16, 0)
    )
    all(tedi.showtimes.map(_.bookingUrl)) shouldBe None
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate.getDayOfMonth) should not contain 28
  }

  it should "carry the row's genres, age rating and runtime" in {
    val mistyczka = movies.find(_.movie.title == "Mistyczka").value
    mistyczka.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 26, 12, 10), LocalDateTime.of(2026, 9, 30, 13, 0), LocalDateTime.of(2026, 10, 1, 14, 20)
    )
    mistyczka.movie.rawTitle.value shouldBe "MISTYCZKA"
    mistyczka.movie.runtimeMinutes.value shouldBe 86
    mistyczka.movie.genres shouldBe Seq("biograficzny", "dramat")
    mistyczka.ageRating.value shouldBe "12+"

    val spider = movies.find(_.movie.title == "Spider-Man: Całkiem Nowy Dzień").value
    spider.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 9, 30, 10, 0))
    spider.movie.runtimeMinutes.value shouldBe 145
    spider.movie.genres shouldBe Seq("akcja", "sci-fi")
    spider.ageRating.value shouldBe "13+"
    spider.movie.rawTitle shouldBe None
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy new KinoEvaClient(new FailingHttpFetch(503)).fetch()
  }
}
