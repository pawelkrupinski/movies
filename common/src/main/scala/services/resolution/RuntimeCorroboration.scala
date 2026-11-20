package services.resolution

/**
 * Whether a candidate film is the one the CINEMAS' own published runtimes describe.
 *
 * Runtime is the evidence [[services.evidence.MixedFilmDetector]] already trusts to tell two films apart: it survives
 * translation, and two prints of one film agree on it while two different films do not. Crucially it is published by
 * the CINEMAS, so an answer drawn from it is not derived from the resolution being questioned (see
 * `MovieRecord.evidence.runtimes`).
 */
object RuntimeCorroboration {

  /** Could a film of `candidate` minutes be the one the cinemas are advertising
   *  at `published` minutes? A VETO, not a choice: it rejects a lone candidate the
   *  runtimes rule out.
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
}
