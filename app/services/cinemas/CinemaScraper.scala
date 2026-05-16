package services.cinemas

import models.{Cinema, CinemaMovie}

/**
 * Single contract every cinema-source obeys: a name (`cinema`) and a thunk
 * (`fetch`) that produces the films currently scheduled at that cinema.
 *
 * Adding a new cinema is a new `CinemaScraper` instance wired in `AppLoader`
 * — `ShowtimeCache` doesn't change (CLAUDE.md OCP guidance). Per-cinema
 * clients implement this directly when they map 1:1 to a cinema; the
 * Cinema City client maps 1:N (Plaza + Kinepolis), so a thin
 * `CinemaCityScraper` wrapper captures the per-cinema parameters.
 */
trait CinemaScraper {
  def cinema: Cinema
  def fetch(): Seq[CinemaMovie]
}

/**
 * Adapter for `CinemaCityClient`, whose `fetch(cinemaId, cinema)` serves
 * multiple cinema variants. Each `CinemaCityScraper` instance captures one
 * (cinemaId, cinema) pair.
 */
class CinemaCityScraper(
  client:   CinemaCityClient,
  cinemaId: String,
  val cinema: Cinema
) extends CinemaScraper {
  def fetch(): Seq[CinemaMovie] = client.fetch(cinemaId, cinema)
}
