package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}

import java.time.LocalDateTime

/**
 * A [[StubCinemaScraper]] that replays a scripted list of outcomes — each either a
 * result (`Right`) or a throw (`Left`) — one per `fetch()` call, plus the stock
 * listings the scraper specs share.
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
      showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-10T18:00"), Some("https://book")))
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

  /** Replays `plan`, one outcome per `fetch()`: a `Right` is the listing, a `Left` is
   *  thrown. Past the end it repeats the last outcome when `repeatLast`, and
   *  otherwise throws — so an unexpected extra call fails the spec. */
  def apply(
    plan:       Seq[Either[Throwable, Seq[CinemaMovie]]],
    forCinema:  Cinema = Multikino,
    repeatLast: Boolean = false
  ): StubCinemaScraper = {
    val script = new Script(plan, repeatLast)
    new StubCinemaScraper(forCinema, script.next())
  }

  private final class Script(plan: Seq[Either[Throwable, Seq[CinemaMovie]]], repeatLast: Boolean) {
    private var played = 0
    def next(): Seq[CinemaMovie] = {
      val outcome = synchronized {
        val i = if (repeatLast) math.min(played, plan.length - 1) else played
        played += 1
        plan.lift(i).getOrElse(throw new IllegalStateException("scripted scraper exhausted"))
      }
      outcome.fold(t => throw t, identity)
    }
  }
}
