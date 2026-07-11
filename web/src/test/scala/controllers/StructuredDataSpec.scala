package controllers

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, Json}

import java.time.LocalDateTime

class StructuredDataSpec extends AnyFlatSpec with Matchers {

  private def ratings(
    imdb: Option[Double] = None, filmweb: Option[Double] = None,
    metascore: Option[Int] = None, rt: Option[Int] = None,
  ) = ResolvedRatings(
    imdb = imdb, imdbUrl = imdb.map(_ => "https://imdb.com/title/tt1"),
    metascore = metascore, metacriticUrl = "https://metacritic.com/x",
    rottenTomatoes = rt, rottenTomatoesUrl = "https://rottentomatoes.com/x",
    filmweb = filmweb, filmwebUrl = "https://filmweb.pl/x",
  )

  private def resolved(title: String, r: ResolvedRatings) = ResolvedMovie(
    _id = title, title = title, originalTitle = None,
    posterUrl = Some("https://img.example/poster.jpg"), fallbackPosterUrls = Nil,
    runtimeMinutes = Some(166), releaseYear = Some(2024),
    genres = Seq("Sci-Fi", "Dramat"), countries = Seq("USA"),
    directors = Seq("Denis Villeneuve"), cast = Seq("Timothée Chalamet", "Zendaya"),
    synopsis = Some("Paul Atryda jednoczy plemiona Fremenów."), trailerUrls = Nil,
    ratings = r, weightedRating = 8.2,
  )

  private def film(
    title: String,
    showtimes: Seq[(Cinema, LocalDateTime, Option[String])],
    r: ResolvedRatings = ratings(imdb = Some(8.5)),
    posterUrl: Option[String] = Some("https://img.example/poster.jpg"),
    synopsis: Option[String] = Some("Paul Atryda jednoczy plemiona Fremenów."),
  ): FilmSchedule = {
    val byDate = showtimes.groupBy(_._2.toLocalDate).toSeq.sortBy(_._1).map { case (d, slots) =>
      val perCinema = slots.groupBy(_._1).toSeq.map { case (cinema, ss) =>
        CinemaShowtimes(cinema, ss.map { case (_, dt, b) => Showtime(dt, b) })
      }
      d -> perCinema
    }
    FilmSchedule(
      movie = Movie(title = title, runtimeMinutes = Some(166), releaseYear = Some(2024),
        countries = Seq("USA"), genres = Seq("Sci-Fi", "Dramat")),
      posterUrl = posterUrl, synopsis = synopsis,
      cast = Seq("Timothée Chalamet", "Zendaya"), director = Seq("Denis Villeneuve"),
      cinemaFilmUrls = Nil, showings = byDate, resolved = resolved(title, r),
    )
  }

  private def parseArray(s: String): Seq[JsObject] =
    Json.parse(s).as[JsArray].value.toSeq.map(_.as[JsObject])

  private def byType(arr: Seq[JsObject], tpe: String): Seq[JsObject] =
    arr.filter(o => (o \ "@type").asOpt[String].contains(tpe))

  // ── landing ────────────────────────────────────────────────────────────────

  "landing JSON-LD" should "describe the WebSite and the Organization in the deployment's language" in {
    import testsupport.TestMessages
    import TestMessages.given   // the deployment's Polish Messages (default Poland)
    val arr = parseArray(StructuredData.landing())
    val site = byType(arr, "WebSite").head
    (site \ "name").as[String]  shouldBe "Kinowo"
    (site \ "url").as[String]   shouldBe "https://kinowo.fly.dev/"
    // The description is the localized landing copy (single source of truth with
    // og:description), NOT the old hardcoded "…w polskich miastach…" literal.
    (site \ "description").as[String] shouldBe TestMessages.forLang("pl")("landing.ogDescription")
    (site \ "description").as[String] should not include "polskich miastach"
    val org = byType(arr, "Organization").head
    (org \ "logo").as[String] should include("og-home.png")
  }

  // ── city page ────────────────────────────────────────────────────────────────

  "cityPage JSON-LD" should "carry a Home › City breadcrumb" in {
    val arr   = parseArray(StructuredData.cityPage("https://kinowo.fly.dev/poznan/", Poznan, Seq(film("Diuna", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None))))))
    val crumb = byType(arr, "BreadcrumbList").head
    val items = (crumb \ "itemListElement").as[JsArray].value
    items.map(i => (i \ "name").as[String])   shouldBe Seq("Kinowo", "Poznań")
    (items(0) \ "item").as[String]            shouldBe "https://kinowo.fly.dev/"
    (items(1) \ "item").as[String]            shouldBe "https://kinowo.fly.dev/poznan/"
    (items(1) \ "position").as[Int]           shouldBe 2
  }

  it should "list the films, sorted, each linking to its detail page" in {
    val films = Seq(
      film("Zorro", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None))),
      film("Amelia", Seq((Helios, LocalDateTime.of(2026, 5, 17, 20, 0), None))),
    )
    val list  = byType(parseArray(StructuredData.cityPage("https://kinowo.fly.dev/poznan/", Poznan, films)), "ItemList").head
    val items = (list \ "itemListElement").as[JsArray].value
    items.map(i => (i \ "name").as[String]) shouldBe Seq("Amelia", "Zorro") // sorted
    (items.head \ "url").as[String]         shouldBe "https://kinowo.fly.dev/poznan/film?title=Amelia"
    (list \ "numberOfItems").as[Int]        shouldBe 2
    (list \ "name").as[String]              shouldBe "Repertuar kin w Poznaniu"
  }

  it should "name the ItemList in the deployment's language (English for a UK city)" in {
    val films = Seq(film("Dune", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None))))
    val list  = byType(parseArray(StructuredData.cityPage("https://kinowo.fly.dev/london/", London, films)), "ItemList").head
    (list \ "name").as[String] shouldBe "Cinema listings in London"
  }

  // ── film page ────────────────────────────────────────────────────────────────

  private val canonical = "https://kinowo.fly.dev/poznan/film?title=Diuna"

  "film JSON-LD" should "emit a Movie with the core fields" in {
    val arr   = parseArray(StructuredData.film(canonical, Poznan, film("Diuna", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)))))
    val movie = byType(arr, "Movie").head
    (movie \ "name").as[String]        shouldBe "Diuna"
    (movie \ "url").as[String]         shouldBe canonical
    (movie \ "description").as[String] should include("Fremenów")
    (movie \ "duration").as[String]    shouldBe "PT166M"
    (movie \ "dateCreated").as[String] shouldBe "2024"
    (movie \ "image").as[String]       shouldBe "https://img.example/poster.jpg"
    (movie \ "genre").as[Seq[String]]  shouldBe Seq("Sci-Fi", "Dramat")
    ((movie \ "director").as[JsArray].value.head \ "name").as[String] shouldBe "Denis Villeneuve"
  }

  it should "carry an aggregateRating from IMDb on a 0–10 scale, with no fabricated count" in {
    val movie = byType(parseArray(StructuredData.film(canonical, Poznan, film("Diuna", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)), r = ratings(imdb = Some(8.5))))), "Movie").head
    val rat   = (movie \ "aggregateRating").as[JsObject]
    (rat \ "ratingValue").as[Double] shouldBe 8.5
    (rat \ "bestRating").as[Int]     shouldBe 10
    (rat \ "ratingCount").asOpt[Int] shouldBe None
    (rat \ "reviewCount").asOpt[Int] shouldBe None
  }

  it should "fall back to Filmweb, then Metacritic on a 0–100 scale" in {
    val fw = byType(parseArray(StructuredData.film(canonical, Poznan, film("D", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)), r = ratings(filmweb = Some(7.3))))), "Movie").head
    ((fw \ "aggregateRating" \ "ratingValue").as[Double], (fw \ "aggregateRating" \ "bestRating").as[Int]) shouldBe (7.3, 10)
    val mc = byType(parseArray(StructuredData.film(canonical, Poznan, film("D", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)), r = ratings(metascore = Some(82))))), "Movie").head
    (mc \ "aggregateRating" \ "bestRating").as[Int] shouldBe 100
  }

  it should "omit aggregateRating when no source has a score" in {
    val movie = byType(parseArray(StructuredData.film(canonical, Poznan, film("D", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)), r = ratings()))), "Movie").head
    (movie \ "aggregateRating").asOpt[JsObject] shouldBe None
  }

  it should "emit one ScreeningEvent per showtime with a zoned startDate and theater location" in {
    val arr = parseArray(StructuredData.film(canonical, Poznan, film("Diuna", Seq(
      (Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), Some("https://book.example/1")),
      (Multikino, LocalDateTime.of(2026, 5, 17, 20, 30), None),
    ))))
    val events = byType(arr, "ScreeningEvent")
    events.size shouldBe 2
    val first = events.head
    (first \ "startDate").as[String] should startWith("2026-05-17T18:00:00+02:00") // Europe/Warsaw DST
    (first \ "location" \ "@type").as[String] shouldBe "MovieTheater"
    (first \ "location" \ "name").as[String]  shouldBe Multikino.displayName
    (first \ "location" \ "address" \ "addressCountry").as[String] shouldBe "PL"
    (first \ "url").as[String] shouldBe "https://book.example/1" // booking link when present
    (events(1) \ "url").as[String] shouldBe canonical            // else the film page
  }

  it should "carry a Home › City › Film breadcrumb" in {
    val crumb = byType(parseArray(StructuredData.film(canonical, Poznan, film("Diuna", Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None))))), "BreadcrumbList").head
    (crumb \ "itemListElement").as[JsArray].value.map(i => (i \ "name").as[String]) shouldBe Seq("Kinowo", "Poznań", "Diuna")
  }

  it should "escape angle brackets so an embedded </script> can't break out" in {
    val raw = StructuredData.film(canonical, Poznan, film("Diuna",
      Seq((Multikino, LocalDateTime.of(2026, 5, 17, 18, 0), None)),
      synopsis = Some("Atak </script><img> w opisie.")))
    raw                 should include("\\u003c/script")
    raw                 should not include "</script>"
    // …and it's still valid JSON that round-trips the original text.
    val movie = byType(parseArray(raw), "Movie").head
    (movie \ "description").as[String] should include("</script>")
  }
}
