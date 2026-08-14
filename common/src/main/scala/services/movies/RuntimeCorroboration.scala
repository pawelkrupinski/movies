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
