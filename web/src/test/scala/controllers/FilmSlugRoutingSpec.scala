package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** Behaviour specific to resolving `/{city}/movie/{slug}` — the lossy-fold cases
 *  the title lookup never had to deal with. */
class FilmSlugRoutingSpec extends AnyFlatSpec with Matchers {

  private def controllerFor(titles: (String, Int)*): MovieController = {
    val now = LocalDateTime.now()
    val rows = titles.map { case (title, year) =>
      val record = MovieRecord(
        imdbId = Some("tt" + math.abs(title.hashCode).toString.take(7)),
        data = Map[Source, SourceData](
          Helios -> SourceData(
            title       = Some(title),
            releaseYear = Some(year),
            posterUrl   = Some("https://cinema.example/poster.jpg"),
            showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
          )
        )
      )
      (title, Some(year), record)
    }
    TestMovieController.build(rows)._1
  }

  private def titleOf(html: String): String =
    "<title>(.*?)</title>".r.findFirstMatchIn(html).map(_.group(1)).getOrElse("")

  "a slug" should "resolve a title whose diacritics and punctuation folded away" in {
    val ctrl = controllerFor(("Diuna: Część druga", 2024))
    val html = contentAsString(
      ctrl.filmBySlug("poznan", "diuna-czesc-druga").apply(FakeRequest(GET, "/poznan/movie/diuna-czesc-druga"))
    )
    titleOf(html) should include("Diuna: Część druga")
  }

  it should "resolve the same film regardless of how the day's showtime ordering shifts" in {
    // Two distinct titles CAN fold onto one slug. `toSchedules` orders by
    // earliest showtime, which moves through the day, so a head-of-list pick
    // would make this URL resolve to a different film at different hours. The
    // resolver tie-breaks on title instead — "Rocky 2" sorts before "Rocky II".
    val ctrl = controllerFor(("Rocky II", 1979), ("Rocky 2", 1979))
    val html = contentAsString(
      ctrl.filmBySlug("poznan", "rocky-2").apply(FakeRequest(GET, "/poznan/movie/rocky-2"))
    )
    titleOf(html) should include("Rocky 2")
  }

  it should "404 rather than fall back to some near-miss film" in {
    val ctrl = controllerFor(("Diuna", 2024))
    val result = ctrl.filmBySlug("poznan", "diuna-czesc-druga")
      .apply(FakeRequest(GET, "/poznan/movie/diuna-czesc-druga"))
    status(result) shouldBe NOT_FOUND
  }

  // The prod bug: `/poznan/movie/zaproszenie` was Olivia Wilde's 2026 "The
  // Invite" AND Wanda Jakubowska's 1986 war drama, because a slug was folded
  // from the title alone. Both films rendered a card, both cards pointed at
  // that one URL, and the 1986 film had no address of its own anywhere in the
  // app. Same shape for `lalka|1968` vs `lalka|2026` in six cities.
  "two genuinely different films sharing a title" should "each get their own address" in {
    val ctrl = controllerFor(("Zaproszenie", 2026), ("Zaproszenie", 1986))

    val newest = contentAsString(
      ctrl.filmBySlug("poznan", "zaproszenie").apply(FakeRequest(GET, "/poznan/movie/zaproszenie")))
    val older = contentAsString(
      ctrl.filmBySlug("poznan", "zaproszenie-1986").apply(FakeRequest(GET, "/poznan/movie/zaproszenie-1986")))

    titleOf(newest) should include("2026")
    titleOf(older)  should include("1986")
  }

  it should "link each card at the film it actually addresses" in {
    val ctrl = controllerFor(("Zaproszenie", 2026), ("Zaproszenie", 1986))
    val html = contentAsString(ctrl.index("poznan").apply(FakeRequest(GET, "/poznan/")))

    val slugs = """data-slug="([^"]*)"""".r.findAllMatchIn(html).map(_.group(1)).toSeq
    slugs should contain allOf ("zaproszenie", "zaproszenie-1986")
    withClue(s"every card must address a different film, got $slugs: ") {
      slugs.distinct should have size slugs.size
    }
  }

  "the og-image route" should "not be swallowed by the slug wildcard" in {
    // Play matches routes top-down, so `/:city/movie/:slug` sitting above
    // `/:city/movie/og-image` would capture "og-image" as a film slug and serve
    // an HTML 404 where every social crawler expects a PNG.
    // Off the classpath, not the filesystem — the spec's working directory
    // differs between an sbt module run and a full-build run.
    val stream = getClass.getResourceAsStream("/routes")
    stream should not be null
    val routes = scala.io.Source.fromInputStream(stream)
    val lines = try routes.getLines().toList finally routes.close()
    val ogImageAt = lines.indexWhere(_.contains("/:city/movie/og-image"))
    val slugAt    = lines.indexWhere(_.contains("/:city/movie/:slug"))
    ogImageAt should be >= 0
    slugAt    should be >= 0
    withClue("og-image must be declared before the :slug wildcard: ") {
      ogImageAt should be < slugAt
    }
  }
}
