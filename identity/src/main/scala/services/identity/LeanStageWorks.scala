package services.identity

import models.CinemaMovie

/** The films a lean archive read keeps the screening DAYS of (`services.scrapes.LeanListing`): those billing a stage
 *  work — a relay's days place it in its season and on its broadcast day ([[Listing.broadcastSeason]],
 *  `agreement.Broadcast`). */
object LeanStageWorks {
  /** Does `film`'s title bill a stage work? */
  def billsStageWork(film: CinemaMovie): Boolean = {
    val raw = film.movie.rawTitle.getOrElse(film.movie.title)
    StageRelays.billsStageWork(Seq(film.movie.title, raw).distinct, raw)
  }
}
