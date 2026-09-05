package services.movies

/**
 * Which of several candidate films the CINEMAS' own published runtimes point at.
 *
 * Two places have to choose between candidate films that a title cannot separate,
 * and both have the same evidence to hand — what the venues printed:
 *
 *   - `MovieCache.chooseConcluded`, deciding which of two same-titled concluded
 *     rows a listing belongs on;
 *   - `FilmCanonicalizer.canonical`, deciding which tmdbId a cluster folded on a
 *     shared imdbId should keep.
 *
 * Runtime is the right evidence for both, and the same evidence
 * [[MixedFilmDetector]] already trusts to tell two films apart: it survives
 * translation, and two prints of one film agree on it while two different films do
 * not. Crucially it is published by the CINEMAS, so an answer drawn from it is not
 * derived from the resolution being questioned (see
 * `MovieRecord.cinemaRuntimesMinutes`).
 *
 * NEAREST, not "agrees within a tolerance", because cinemas round and pad: for
 * "Tylko jedna noc" TMDB says 102 minutes while Multikino publishes 105 and Cinema
 * City 102, so a ±2 agreement test answers "neither film" for Multikino. Against a
 * same-titled 1961 picture running 121, the distances (3 against 16) are not a close
 * call.
 */
object RuntimeCorroboration {

  /** Could a film of `candidate` minutes be the one the cinemas are advertising
   *  at `published` minutes? A VETO, not a choice: [[strictNearest]] picks between
   *  rival films, this one rejects a lone candidate the runtimes rule out.
   *
   *  Deliberately a wide band rather than a tolerance, because cinemas publish
   *  runtimes loosely — padded with ads, rounded, or shaved (Multikino advertises
   *  the 162-minute "Lalka" at 147). Anything within half to double what a venue
   *  printed stays plausible; the band exists only to catch a match that is a
   *  different KIND of thing. Prod resolved `vivaldiija|2023` to an 18-minute
   *  concert short while 46 venues screened the 110-minute feature, and
   *  `homosapiens|1960` to a 9-minute animated short against a 95-minute listing.
   *  Nothing in a cinema's rounding turns 110 into 18.
   *
   *  Abstains — true — whenever either side published nothing, so a film no venue
   *  timed is never rejected for want of evidence. */
  def plausible(published: Iterable[Int], candidate: Option[Int]): Boolean = {
    val minutes = published.filter(_ > 0)
    (minutes.minOption, minutes.maxOption, candidate.filter(_ > 0)) match {
      case (Some(shortest), Some(longest), Some(own)) => own * 2 >= shortest && own <= longest * 2
      case _                                          => true
    }
  }

  /** The candidate the published runtimes sit STRICTLY nearest.
   *
   *  `None` when nothing was published, when no candidate carries a runtime of its
   *  own, or when two candidates are equally far away — an equidistant pair says
   *  nothing about which film this is, and a caller must fall back to other evidence
   *  rather than break the tie arbitrarily. */
  def strictNearest[A](published: Iterable[Int], candidates: Seq[(A, Option[Int])]): Option[A] = {
    val distances = for {
      (candidate, ownRuntime) <- candidates
      minutes                 <- ownRuntime
      nearest                 <- published.map(p => math.abs(p - minutes)).minOption
    } yield candidate -> nearest
    distances.minByOption(_._2).map(_._2) match {
      case Some(best) if distances.count(_._2 == best) == 1 =>
        distances.collectFirst { case (candidate, distance) if distance == best => candidate }
      case _ => None
    }
  }
}
