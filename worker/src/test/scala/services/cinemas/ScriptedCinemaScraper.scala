package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}
import services.cinemas.common.CinemaScraper

import java.time.LocalDateTime

/**
 * Test double: a `CinemaScraper` that replays a scripted list of outcomes —
 * each either a result (`Right`) or a throw (`Left`) — one per `fetch()` call.
 * Shared by the retry + recording decorator specs so the scripting shape isn't
 * re-spelled per spec.
 */
object ScriptedCinemaScraper {

  /** One film with a single showtime — a non-empty, "green" scrape result. */
  val OneMovie: Seq[CinemaMovie] = Seq(
    CinemaMovie(
      movie     = Movie("X"),
      cinema    = Multikino,
      posterUrl = None,
      filmUrl   = None,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.now(), Some("https://book")))
    )
  )

  /** A film the page surfaced but with no showtimes — zero screenings, same as
   *  an empty result. */
  val NoShowtimes: Seq[CinemaMovie] = Seq(
    CinemaMovie(
      movie     = Movie("X"),
      cinema    = Multikino,
      posterUrl = None,
      filmUrl   = None,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = Seq.empty
    )
  )

  def apply(
    plan:      List[Either[Throwable, Seq[CinemaMovie]]],
    forCinema: Cinema = Multikino
  ): CinemaScraper = new CinemaScraper {
    private var remaining = plan
    val cinema: Cinema           = forCinema
    def scrapeHosts: Set[String] = Set.empty
    def fetch(): Seq[CinemaMovie] = remaining match {
      case Right(v) :: rest => remaining = rest; v
      case Left(t)  :: rest => remaining = rest; throw t
      case Nil              => throw new IllegalStateException("scripted scraper exhausted")
    }
  }
}
