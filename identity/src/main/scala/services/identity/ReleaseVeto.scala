package services.identity

import services.identity.IdentityMeasures.{Measure, Number}

/** The RELEASE VETO: among the films a listing's bare title names exactly (its NAMESAKES), one TMDB dates no release of in the venue's country, while it dates one of a
 *  namesake beside it, is not the venue's film — unless something backs it: the facts the listing published, on balance
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
 *  retrospective, a club night) — that relation is no namesake, but for a release of the very season ([[ofBanner]]). One
 *  rule for every country: the country is the venue's, an input. */
private[identity] object ReleaseVeto {
  /** The namesakes among `scored` vetoed at a venue in `country` (ISO-3166-1), each with why; `namesake` says which
   *  candidates are namesakes, `backed` which of them something the listing or another venue published backs. Most
   *  pools have no namesake TMDB dates no release of there: they are told so without building anything. */
  def of(country: String, scored: Seq[Scored], namesake: Scored => Boolean, backed: Scored => Boolean): Map[Int, Denial] = {
    def released(s: Scored, in: Boolean) = s.candidate.film.knownReleasedIn(country, in)
    if (!scored.exists(s => released(s, in = false) && namesake(s))) Map.empty
    else scored.find(s => released(s, in = true) && namesake(s)).fold(Map.empty[Int, Denial]) { shown =>
      scored.iterator.filter(s => released(s, in = false) && namesake(s) && !backed(s))
        .map(s => s.candidate.tmdbId -> why(country, shown.candidate.tmdbId)).toMap
    }
  }

  /** The BANNER form, for a title naming no film whole and one work beside its banner (`piece`: the films it names by a
   *  piece): among the films `named` by that segment,
   *  one TMDB dates no release of in `country`, beside one it dates a CINEMA release of there in the `season` the listing
   *  screens in. A programme shows films with no release there as a matter of course (a sneak, a retrospective), so only
   *  a release of that very season says which it is. Backed only by the facts the listing published, and a running
   *  time the released film fits too backs nothing: a slot runs the film and more ([[Corroboration.fits]]). UK Mowlem's
   *  "Purbeck Film Festival - Hen" [110′], October 2026: Pálfi's 96′ film opened in Britain that February, the 108′
   *  Afrikaans "Hen" never did. */
  def ofBanner(country: String, season: Option[Int], scored: Seq[Scored], named: Scored => Boolean,
               piece: Scored => Boolean, backed: (Scored, Set[String]) => Boolean): Map[Int, Denial] = season.filter(_ =>
    // one work beside the banner: every film named by a piece named by that one piece. PL "Lalka (DOLLY)" bills two
    // works, UK "The Metropolitan Opera: Macbeth" names the Met's record by its house as well as another's by the work
    scored.iterator.filter(piece).map(s => IdentityMeasures.namingPieces(s.listing, s.candidate.film)).distinct.take(2).size == 1
  ).fold(Map.empty[Int, Denial]) { season =>
    scored.find(s => named(s) && cinemaReleases(s, country).exists(_.year == season)).fold(Map.empty[Int, Denial]) { shown =>
      val silent = if (Corroboration.fits(shown)) Set("runtime.delta") else Set.empty[String]
      scored.iterator.filter(s => named(s) && s.candidate.film.knownReleasedIn(country, dated = false) && !backed(s, silent))
        .map(s => s.candidate.tmdbId -> why(country, shown.candidate.tmdbId)).toMap
    }
  }

  private def cinemaReleases(s: Scored, country: String) =
    s.candidate.film.releases.fold(Seq.empty[TmdbFilmRecord.Release])(TmdbFilmRecord.Release.all).filter(_.country == country)

  /** The CURRENT-RELEASE form: among the namesakes, one TMDB dates a CINEMA release of in the venue's `country` in the
   *  season the listing screens in (or the year after: a release dated early in the next year screens in that season),
   *  and another, not itself current (dated before last year) but recent ([[StreamingEraYears]]), TMDB dates releases of but no cinema release in that country —
   *  a streaming or video title there. The listing screens the one its country's cinemas are showing now. Backed only by
   *  facts the venue states itself (`ownFacts`, weighing for it on balance in `backed`): a feed's copied facts are its
   *  entry's claim of which film it is ([[SourceReliability]]), and other venues' credits back a remake by the same
   *  director as much as its original. DE City-Kinos' and KommKino's "The Killer", Filmstarts and kinoheld copying the
   *  2024 Peacock remake's year: TMDB dates Woo's 1989 film's German re-release on 2026-10-29, the week they screen it,
   *  and only a digital German release of the remake. */
  def ofCurrent(country: String, season: Option[Int], scored: Seq[Scored], namesake: Scored => Boolean, ownFacts: Boolean,
                backed: Scored => Boolean): Map[Int, Denial] = season.fold(Map.empty[Int, Denial]) { season =>
    def streamedOnly(s: Scored) = s.candidate.film.releases.isDefined && s.candidate.film.knownReleasedIn(country, dated = true) &&
      cinemaReleases(s, country).isEmpty &&
      s.candidate.film.year.exists(year => year < season - 1 && year >= season - StreamingEraYears)
    if (!scored.exists(s => namesake(s) && streamedOnly(s))) Map.empty
    else scored.find(s => namesake(s) && cinemaReleases(s, country).exists(r => r.year == season || r.year == season + 1)).fold(Map.empty[Int, Denial]) { shown =>
      scored.iterator.filter(s => s.candidate.tmdbId != shown.candidate.tmdbId && namesake(s) && streamedOnly(s) && !(ownFacts && backed(s)))
        .map(s => s.candidate.tmdbId -> why(country, shown.candidate.tmdbId)).toMap
    }
  }

  /** How many seasons back a namesake [[ofCurrent]] vetoes may date from: TMDB records a recent film's releases by type
   *  in each country, a streaming service's among them, but an older film's sparsely — ES Serantes' "De repente" {Lewis
   *  Allen} [1954] has no Spanish cinema release on record and is the film all the same. */
  val StreamingEraYears = 5

  /** The denial a vetoed namesake carries — the decision's explanation names the veto and the namesake it stands beside. */
  def why(country: String, released: Int): Denial =
    Denial(s"vetoed: not released in $country, as namesake $released is", Denial.Kind.Vetoed(released))
  /** Is `denial` this veto's? */
  def vetoes(denial: Denial): Boolean = releasedBeside(denial).isDefined
  /** The namesake released in the venue's country that this veto's `denial` stood the vetoed film beside ([[why]]). */
  def releasedBeside(denial: Denial): Option[Int] = denial.kind match {
    case Denial.Kind.Vetoed(released) => Some(released)
    case _                            => None
  }

  /** `measures` with the namesakes the veto took away from `scored`'s rivals taken out of its `rivals` count — never
   *  below none: the veto counts every namesake it took away, which can be more than `rivals` ranked against the film. */
  def unvetoed(scored: Scored, measures: Map[String, Measure]): Map[String, Measure] =
    if (scored.vetoedRivals == 0) measures
    else measures.get("rivals").collect { case Number(rivals) => measures.updated("rivals", Number(math.max(0.0, rivals - scored.vetoedRivals))) }
      .getOrElse(measures)
}
