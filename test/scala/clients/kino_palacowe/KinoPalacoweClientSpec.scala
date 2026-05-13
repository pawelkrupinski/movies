package clients.kino_palacowe

import clients.KinoPalacoweClient
import clients.tools.FakeHttpFetch
import models.{KinoPalacowe, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class KinoPalacoweClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "KinoPalacoweClient.fetch" should "return exactly 5 movies from fixture" in {
    results.size shouldBe 5
  }

  it should "return 6 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 6
  }

  it should "assign KinoPalacowe cinema to all entries" in {
    results.map(_.cinema).toSet shouldBe Set(KinoPalacowe)
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of movie titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Chłopiec na krańcach świata",
      "Giulietta i duchy",
      "Głos z księżyca",
      "Osiem i pół",
      "Słodkie życie",
    )
  }

  // ── Runtime (all movies) ──────────────────────────────────────────────────

  it should "return correct runtime for every movie" in {
    val runtimes = results.map(m => m.movie.title -> m.movie.runtimeMinutes).toMap
    runtimes("Chłopiec na krańcach świata") shouldBe Some(88)
    runtimes("Giulietta i duchy")           shouldBe Some(43)
    runtimes("Głos z księżyca")             shouldBe Some(121)
    runtimes("Osiem i pół")                 shouldBe Some(138)
    runtimes("Słodkie życie")               shouldBe Some(176)
  }

  // ── Poster URLs ───────────────────────────────────────────────────────────

  it should "return correct poster URL for every movie" in {
    val posters = results.map(m => m.movie.title -> m.posterUrl).toMap
    posters("Chłopiec na krańcach świata") shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/Chopiec_na_krancach_swiata_PLAKAT_teaser.jpg")
    posters("Giulietta i duchy")           shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/Giulietta_degli_2.jpg")
    posters("Głos z księżyca")             shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/La_voce_della_lunba_3.jpg")
    posters("Osiem i pół")                 shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/still2_zqnlD61.jpg")
    posters("Słodkie życie")               shouldBe Some("https://kinopalacowe.pl/media/gallery/lg/still_4_PbgzW23.jpg")
  }

  // ── Film URLs ─────────────────────────────────────────────────────────────

  it should "return correct film URL for every movie" in {
    val filmUrls = results.map(m => m.movie.title -> m.filmUrl).toMap
    filmUrls("Chłopiec na krańcach świata") shouldBe Some("http://kinopalacowe.pl/filmy/14435-poranek-dla-dzieci-chopiec-na-krancach-swiata/")
    filmUrls("Giulietta i duchy")           shouldBe Some("http://kinopalacowe.pl/filmy/14403-giulietta-i-duchy-federico-fellini-ciao-a-tutti/")
    filmUrls("Głos z księżyca")             shouldBe Some("http://kinopalacowe.pl/filmy/14404-gos-z-ksiezyca-federico-fellini-ciao-a-tutti/")
    filmUrls("Osiem i pół")                 shouldBe Some("http://kinopalacowe.pl/filmy/14402-osiem-i-po-federico-fellini-ciao-a-tutti/")
    filmUrls("Słodkie życie")               shouldBe Some("http://kinopalacowe.pl/filmy/14401-sodkie-zycie-federico-fellini-ciao-a-tutti/")
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count for every movie" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Chłopiec na krańcach świata") shouldBe 2
    counts("Giulietta i duchy")           shouldBe 1
    counts("Głos z księżyca")             shouldBe 1
    counts("Osiem i pół")                 shouldBe 1
    counts("Słodkie życie")               shouldBe 1
  }

  // ── Full showtime details ─────────────────────────────────────────────────

  it should "return exact showtimes for Chłopiec na krańcach świata" in {
    val st = byTitle("Chłopiec na krańcach świata").showtimes
    st.size shouldBe 2
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 6, 27, 11, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=925494"), Some("Sala 1 - Kinowa"), None),
      Showtime(LocalDateTime.of(2026, 6, 28, 11, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=925496"), Some("Sala 1 - Kinowa"), None),
    )
  }

  it should "return exact showtime for Głos z księżyca" in {
    val st = byTitle("Głos z księżyca").showtimes
    st.size shouldBe 1
    st shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 7, 17, 18, 0), Some("https://ckzamek.bilety24.pl/kup-bilety/?id=924779"), Some("Sala 1 - Kinowa"), None),
    )
  }
}
