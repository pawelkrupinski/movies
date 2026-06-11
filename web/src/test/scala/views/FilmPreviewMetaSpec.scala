package views

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Helios, Movie, MovieRecord, Poznan, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

// The film page's Open Graph / Twitter-card metadata drives the link
// preview WhatsApp / Messenger / Slack / Telegram / X render when the
// /film URL is shared. Movie posters are 2:3 portrait, so the preview
// must NOT force a 1.91:1 landscape crop (which lops off the poster's
// top + bottom). This spec pins the tags that keep the whole poster
// visible alongside the title + rating text.
class FilmPreviewMetaSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val sample: FilmSchedule =
    FilmSchedule(
      movie          = Movie("Incepcja", runtimeMinutes = Some(148), releaseYear = Some(2010), genres = Seq("Sci-Fi")),
      posterUrl      = Some("https://image.tmdb.org/t/p/original/incepcja.jpg"),
      synopsis       = Some("Dom Cobb wykrada sekrety z podświadomości podczas snu."),
      cast           = Seq("Leonardo DiCaprio"),
      director       = Seq("Christopher Nolan"),
      cinemaFilmUrls = Nil,
      showings       = Seq(LocalDate.of(2026, 6, 4) -> Seq(CinemaShowtimes(Helios, Seq(
        Showtime(LocalDateTime.of(2026, 6, 4, 18, 0), Some("https://example.test/book"), Some("Sala 1"), List("2D"))
      )))),
      enrichment     = Some(MovieRecord(imdbId = Some("tt1375666"), imdbRating = Some(8.8), rottenTomatoes = Some(87)))
    )

  private def render(film: FilmSchedule): String =
    views.html.film(film, "https://kinowo.fly.dev/poznan/film?title=Incepcja",
      ogDescription = "IMDb 8.8 · RT 87% — synopsis", devMode = false).body

  "the film preview" should "use the square `summary` twitter card, not the landscape `summary_large_image` (which crops the 2:3 poster)" in {
    val html = render(sample)
    html should     include ("""<meta name="twitter:card"  content="summary">""")
    html should not include "summary_large_image"
  }

  it should "declare the poster's portrait dimensions so scrapers lay it out whole instead of assuming landscape" in {
    val html = render(sample)
    html should include ("""<meta property="og:image:width"  content="480">""")
    html should include ("""<meta property="og:image:height" content="720">""")
  }

  it should "point og:image and twitter:image at the proxied poster" in {
    val html = render(sample)
    // Twirl HTML-escapes the `&` query-param separators in the attribute
    // value, so the rendered tag carries `&amp;` where PosterProxy emits `&`.
    val proxied = tools.PosterProxy.proxy("https://image.tmdb.org/t/p/original/incepcja.jpg").replace("&", "&amp;")
    html should include (s"""<meta property="og:image"        content="$proxied">""")
    html should include (s"""<meta name="twitter:image" content="$proxied">""")
  }

  it should "still carry the title in og:title and twitter:title" in {
    val html = render(sample)
    html should include ("""<meta property="og:title"       content="Incepcja">""")
    html should include ("""<meta name="twitter:title" content="Incepcja">""")
  }

  it should "omit og:image dimension tags when the film has no poster" in {
    val html = render(sample.copy(posterUrl = None))
    html should not include "og:image:width"
    html should not include "og:image:height"
  }
}
