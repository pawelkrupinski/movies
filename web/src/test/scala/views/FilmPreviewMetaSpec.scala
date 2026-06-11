package views

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Helios, Movie, MovieRecord, Poznan, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

// The film page's Open Graph / Twitter-card metadata drives the link
// preview WhatsApp / Messenger / Slack / Telegram / X render when the
// /film URL is shared. og:image points at a server-rendered 1200×630
// composite (full poster + title + ratings) so the preview UIs can't crop
// the poster out and the ratings are always visible. This spec pins those
// tags.
class FilmPreviewMetaSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val ogImageUrl = "https://kinowo.fly.dev/poznan/film/og-image?title=Incepcja"

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

  private def render(film: FilmSchedule, imageUrl: String = ogImageUrl): String =
    views.html.film(film, "https://kinowo.fly.dev/poznan/film?title=Incepcja",
      ogDescription = "IMDb 8.8 · RT 87% — synopsis", ogImageUrl = imageUrl, devMode = false).body

  "the film preview" should "point og:image + twitter:image at the server-rendered composite card" in {
    val html = render(sample)
    html should include (s"""<meta property="og:image"        content="$ogImageUrl">""")
    html should include (s"""<meta name="twitter:image" content="$ogImageUrl">""")
  }

  it should "declare the card as a 1200×630 PNG so previews render it large and uncropped" in {
    val html = render(sample)
    html should include ("""<meta property="og:image:type"   content="image/png">""")
    html should include ("""<meta property="og:image:width"  content="1200">""")
    html should include ("""<meta property="og:image:height" content="630">""")
  }

  it should "use the large-image twitter card (the composite is already 1.91:1, so nothing is cropped)" in {
    val html = render(sample)
    html should include ("""<meta name="twitter:card"  content="summary_large_image">""")
  }

  it should "still carry the title in og:title and twitter:title" in {
    val html = render(sample)
    html should include ("""<meta property="og:title"       content="Incepcja">""")
    html should include ("""<meta name="twitter:title" content="Incepcja">""")
  }

  it should "omit the image tags entirely when no card URL is supplied" in {
    val html = render(sample, imageUrl = "")
    html should not include "og:image"
    html should not include "twitter:image"
  }
}
