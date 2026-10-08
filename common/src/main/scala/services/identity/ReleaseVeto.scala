package services.identity

import services.identity.IdentityMeasures.{Measure, Number}

/** The RELEASE VETO: among the films a listing's bare title names exactly (its NAMESAKES), one TMDB dates no release of in the venue's country, while it dates one of a
 *  namesake beside it, is not the venue's film — unless something backs it: a fact the listing published
 *  (`EvidenceWeights.factsSupport`) or another venue's own facts (`venues.corroborating`). Helios's bare "Diabły" is Ken
 *  Russell's 1971 film, whose Polish release TMDB does not date, because 43 venues crediting him say so.
 *
 *  The veto adds no evidence FOR any film: a vetoed namesake is denied, and is no rival in the evidence CLASS the film
 *  left is accepted by (an exact top hit with no rival, [[unvetoed]]); the calibrated probability still counts it, so
 *  a candidate the listing barely names gains nothing from the veto. PL Kino Kameralne Cafe's bare "Obcy w domu":
 *  TMDB's first hit, the 1986 Polish film released in Poland, was held off its class by a 1989 US namesake TMDB dates
 *  no Polish release of.
 *
 *  It never decides alone: a namesake whose release dates were not fetched, or a pool where no namesake is released in
 *  the country, vetoes nothing; nor does a title naming its film only by a piece (a programme's banner beside it — a
 *  retrospective, a club night) — that relation is no namesake. One rule for every country: the country is the
 *  venue's, an input. */
private[identity] object ReleaseVeto {
  /** The namesakes among `scored` vetoed at a venue in `country` (ISO-3166-1), each with why; `namesake` says which
   *  candidates are namesakes, `backed` which of them something the listing or another venue published backs. Most
   *  pools have no namesake TMDB dates no release of there: they are told so without building anything. */
  def of(country: String, scored: Seq[Scored], namesake: Scored => Boolean, backed: Scored => Boolean): Map[Int, String] = {
    def released(s: Scored, in: Boolean) = s.candidate.film.knownReleasedIn(country, in)
    if (!scored.exists(s => released(s, in = false) && namesake(s))) Map.empty
    else scored.find(s => released(s, in = true) && namesake(s)).fold(Map.empty[Int, String]) { shown =>
      scored.iterator.filter(s => released(s, in = false) && namesake(s) && !backed(s))
        .map(s => s.candidate.tmdbId -> why(country, shown.candidate.tmdbId)).toMap
    }
  }

  /** The denial a vetoed namesake carries — the decision's explanation names the veto and the namesake it stands beside. */
  def why(country: String, released: Int): String = s"$Vetoed $country, as namesake $released is"
  private val Vetoed = "vetoed: not released in"
  /** Is `denial` this veto's? */
  def vetoes(denial: String): Boolean = denial.startsWith(Vetoed)

  /** `measures` with the namesakes the veto took away from `scored`'s rivals taken out of its `rivals` count — never
   *  below none: the veto counts every namesake it took away, which can be more than `rivals` ranked against the film. */
  def unvetoed(scored: Scored, measures: Map[String, Measure]): Map[String, Measure] =
    if (scored.vetoedRivals == 0) measures
    else measures.get("rivals").collect { case Number(rivals) => measures.updated("rivals", Number(math.max(0.0, rivals - scored.vetoedRivals))) }
      .getOrElse(measures)
}
