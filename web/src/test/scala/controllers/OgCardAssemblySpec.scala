package controllers

import models.{Movie, MovieRecord, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

/** Pins the share-preview view-model assembly extracted out of MovieController
 *  into [[OgCardAssembly]] — the og:description text and the city-card film
 *  dedup / daily rotation. Behaviour-preserving: these assertions moved here
 *  verbatim from CityOgImageControllerSpec (dedup/rotation) plus added cover
 *  for the og:description builders the extraction newly isolated. */
class OgCardAssemblySpec extends AnyFlatSpec with Matchers {

  private def sched(title: String, poster: Option[String] = None): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = poster, synopsis = None, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil, resolved = TestReadModel.resolved(title, None, MovieRecord()))

  private def ratedSched(title: String, ratings: ResolvedRatings, synopsis: Option[String] = None): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = None, synopsis = synopsis, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil,
      resolved = TestReadModel.resolved(title, None, MovieRecord()).copy(ratings = ratings))

  private def ratings(imdb: Option[Double] = None, metascore: Option[Int] = None,
                      rt: Option[Int] = None, filmweb: Option[Double] = None): ResolvedRatings =
    ResolvedRatings(
      imdb = imdb, imdbUrl = None,
      metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
      rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
      filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
    )

  "distinctByMovie" should "collapse a film's programme variant so its poster isn't repeated" in {
    // A base showing + its accessibility variant share an upstream search key
    // ("Kino bez barier: Freak Show" → "Freak Show"), so they're one film/poster.
    val out = OgCardAssembly.distinctByMovie(Seq(
      sched("Freak Show"), sched("Kino bez barier: Freak Show"), sched("Toy Story 5"))
    ).map(_.movie.title)
    out shouldBe Seq("Freak Show", "Toy Story 5")
  }

  it should "drop unrelated films that share one poster image (generic placeholder)" in {
    // A retrospective where several distinct titles all use one placeholder
    // poster — keep only the first so the image isn't repeated.
    val out = OgCardAssembly.distinctByMovie(Seq(
      sched("Ziemia obiecana", Some("https://cdn/generic.jpg")),
      sched("Brzezina",        Some("https://cdn/generic.jpg")),
      sched("Toy Story 5",     Some("https://cdn/toy.jpg")))
    ).map(_.movie.title)
    out shouldBe Seq("Ziemia obiecana", "Toy Story 5")
  }

  "dailyCardFilms" should "rotate to a different, non-overlapping set each day (stable within a day)" in {
    val pool = (1 to 12).map(i => sched(s"Film $i", Some(s"https://cdn/$i.jpg")))
    val d1 = OgCardAssembly.dailyCardFilms(pool, epochDay = 100, count = 5).map(_.movie.title)
    val d2 = OgCardAssembly.dailyCardFilms(pool, epochDay = 101, count = 5).map(_.movie.title)
    d1 should have size 5
    d2 should have size 5
    d1 should not equal d2              // a different day shows a different set
    d1.intersect(d2) shouldBe empty     // count-per-day step → adjacent days disjoint
    OgCardAssembly.dailyCardFilms(pool, 100, 5).map(_.movie.title) shouldBe d1 // deterministic
  }

  it should "drop films without a poster (no grey slots) and survive an empty pool" in {
    OgCardAssembly.dailyCardFilms(Seq(sched("No poster", None)), epochDay = 1, count = 5) shouldBe empty
  }

  it should "cycle through the FULL repertoire, not just the first 40" in {
    val pool = (1 to 50).map(i => sched(s"Film $i", Some(s"https://cdn/$i.jpg")))
    // epochDay 9, count 5 → window starts at index 45, so films 46–50 show —
    // unreachable when the pool was capped at the first 40.
    OgCardAssembly.dailyCardFilms(pool, epochDay = 9, count = 5).map(_.movie.title) should contain ("Film 50")
  }

  "ratingTokens" should "emit a token per set source, skipping the unset ones" in {
    OgCardAssembly.ratingTokens(ratedSched("X", ratings(imdb = Some(8.8), rt = Some(87)))) shouldBe
      Seq("IMDb 8.8", "RT 87%")
    OgCardAssembly.ratingTokens(ratedSched("Y",
      ratings(imdb = Some(7.0), metascore = Some(74), rt = Some(90), filmweb = Some(6.5)))) shouldBe
      Seq("IMDb 7.0", "RT 90%", "Metacritic 74", "Filmweb 6.5")
    OgCardAssembly.ratingTokens(ratedSched("Z", ratings())) shouldBe empty
  }

  "previewDescription" should "join the rating summary and synopsis, or fall back to whichever is present" in {
    OgCardAssembly.previewDescription(
      ratedSched("X", ratings(imdb = Some(8.8)), synopsis = Some("Sen w śnie."))) shouldBe
      "IMDb 8.8 — Sen w śnie."
    OgCardAssembly.previewDescription(ratedSched("X", ratings(imdb = Some(8.8)))) shouldBe "IMDb 8.8"
    OgCardAssembly.previewDescription(
      ratedSched("X", ratings(), synopsis = Some("Tylko opis."))) shouldBe "Tylko opis."
    OgCardAssembly.previewDescription(ratedSched("X", ratings())) shouldBe ""
  }
}
