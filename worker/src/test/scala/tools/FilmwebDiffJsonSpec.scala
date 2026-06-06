package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

import java.time.LocalDateTime

/** Pins the deterministic JSON `FilmwebDiff` writes as its third output: cinemas
 *  sorted by (city, cinema), per-film discrepancies sorted by title with SORTED
 *  ISO times, exact-match films omitted, and `summary` counts derived from the
 *  per-cinema verdicts. Pure — feeds synthetic `CinemaResult`s through the
 *  renderer, no network. */
class FilmwebDiffJsonSpec extends AnyFlatSpec with Matchers {
  import FilmwebDiffJson.{CinemaResult, Meta, render}

  private def dt(s: String): LocalDateTime = LocalDateTime.parse(s)

  private val meta = Meta(
    date = "2026-06-06", generatedAt = "2026-06-06T19:12:00Z", windowDays = 3, commit = "abc123"
  )

  // Krakow cinema: one film differs (ours-only + fw-only), one film matches exactly.
  private val kazimierz = CinemaResult(
    city = "krakow", cinema = "Cinema City Kazimierz", filmwebId = Some(564),
    resolvedVia = "fuzzy", verdict = "BOTH_DIFFER",
    oursByFilm = Map(
      "diabel ubiera sie u prady 2" -> Seq(dt("2026-06-06T21:50"), dt("2026-06-07T21:50"), dt("2026-06-06T14:00")),
      "matrix"                      -> Seq(dt("2026-06-06T18:00"))
    ),
    fwByFilm = Map(
      "diabel ubiera sie u prady 2" -> Seq(dt("2026-06-06T21:30"), dt("2026-06-06T18:30"), dt("2026-06-06T14:00")),
      "matrix"                      -> Seq(dt("2026-06-06T18:00"))
    )
  )

  // Warszawa cinema, all identical → no discrepancies, verdict SAME. Listed
  // BEFORE kazimierz here to prove the renderer re-sorts by (city, cinema).
  private val luna = CinemaResult(
    city = "warszawa", cinema = "Kino Luna", filmwebId = Some(900),
    resolvedVia = "override", verdict = "SAME",
    oursByFilm = Map("dune" -> Seq(dt("2026-06-06T20:00"))),
    fwByFilm   = Map("dune" -> Seq(dt("2026-06-06T20:00")))
  )

  // No Filmweb id: counts 0, empty discrepancies, still present so the set is stable.
  private val muzeum = CinemaResult(
    city = "krakow", cinema = "Kino Muzeum", filmwebId = None,
    resolvedVia = "none", verdict = "NO_FILMWEB_ID",
    oursByFilm = Map.empty, fwByFilm = Map.empty
  )

  private val results = Seq(luna, kazimierz, muzeum)

  "render" should "produce valid JSON with the documented top-level shape" in {
    val js = render(results, meta)
    (js \ "meta" \ "date").as[String]        shouldBe "2026-06-06"
    (js \ "meta" \ "generatedAt").as[String] shouldBe "2026-06-06T19:12:00Z"
    (js \ "meta" \ "windowDays").as[Int]     shouldBe 3
    (js \ "meta" \ "commit").as[String]      shouldBe "abc123"
    // round-trips through a parser → it really is valid JSON
    Json.parse(Json.stringify(js)) shouldBe js
  }

  it should "sort cinemas by (city, cinema)" in {
    val cinemas = (render(results, meta) \ "cinemas").as[JsArray].value
    cinemas.map(c => (c \ "city").as[String] -> (c \ "cinema").as[String]) shouldBe Seq(
      "krakow"   -> "Cinema City Kazimierz",
      "krakow"   -> "Kino Muzeum",
      "warszawa" -> "Kino Luna"
    )
  }

  it should "list a differing film in discrepancies with SORTED ISO times, and omit exact-match films" in {
    val kaz = cinemaNamed("Cinema City Kazimierz")
    val disc = (kaz \ "discrepancies").as[JsArray].value
    // Only the differing film — "matrix" matches exactly and must be absent.
    disc.map(d => (d \ "film").as[String]) shouldBe Seq("diabel ubiera sie u prady 2")

    val d = disc.head
    (d \ "ours").as[Int]   shouldBe 3
    (d \ "fw").as[Int]     shouldBe 3
    (d \ "shared").as[Int] shouldBe 1 // only 14:00 is shared
    (d \ "oursOnly").as[Seq[String]] shouldBe Seq("2026-06-06T21:50", "2026-06-07T21:50")
    (d \ "fwOnly").as[Seq[String]]   shouldBe Seq("2026-06-06T18:30", "2026-06-06T21:30")
  }

  it should "give an all-identical cinema an empty discrepancies array" in {
    (cinemaNamed("Kino Luna") \ "discrepancies").as[JsArray].value shouldBe empty
  }

  it should "include a NO_FILMWEB_ID cinema with null id, zero counts, empty discrepancies" in {
    val m = cinemaNamed("Kino Muzeum")
    (m \ "filmwebId").asOpt[Int]   shouldBe None
    (m \ "filmwebId").get          shouldBe JsNull
    (m \ "ours").as[Int]           shouldBe 0
    (m \ "fw").as[Int]             shouldBe 0
    (m \ "verdict").as[String]     shouldBe "NO_FILMWEB_ID"
    (m \ "discrepancies").as[JsArray].value shouldBe empty
  }

  it should "derive summary counts from the per-cinema verdicts" in {
    val s = render(results, meta) \ "summary"
    (s \ "cinemas").as[Int]     shouldBe 3
    (s \ "same").as[Int]        shouldBe 1
    (s \ "bothDiffer").as[Int]  shouldBe 1
    (s \ "noFilmwebId").as[Int] shouldBe 1
    (s \ "oursExtra").as[Int]   shouldBe 0
    (s \ "fwExtra").as[Int]     shouldBe 0
    (s \ "fetchFailed").as[Int] shouldBe 0
  }

  it should "be deterministic — identical input renders byte-identical output regardless of cinema order" in {
    Json.stringify(render(results, meta)) shouldBe Json.stringify(render(results.reverse, meta))
  }

  private def cinemaNamed(name: String): JsValue =
    (render(results, meta) \ "cinemas").as[JsArray].value
      .find(c => (c \ "cinema").as[String] == name)
      .getOrElse(fail(s"cinema $name not in output"))
}
