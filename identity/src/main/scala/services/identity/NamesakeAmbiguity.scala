package services.identity

/** NAMESAKE-AMBIGUOUS listings (docs/investigations/2026-11-04-namesake-ambiguous.md; the owner, 2026-11-04: "a take is a
 *  pure function of the current inputs"; "a flip is a wrong match to eliminate"; "unmatched beats wrong"; "near-ties stay
 *  unmatched").
 *
 *  A listing is namesake-ambiguous when TMDB's own candidates for its title hold two plausible works or more -- works its
 *  title names (or IMDb lists under it), of a kind no rule of its own rules out ([[plausible]]) -- and none of its OWN
 *  facts singles one out ([[singledOut]]): no pin, no TMDB-keyed feed link, and no year, director (its own credit or its
 *  text's), original title, discriminating running time, season, cast, stated original title or release in its country
 *  telling one apart. Both are read on a family scope of its node ALONE: the node's own searches' candidates, scored
 *  against its own facts, never a sibling's. So whether a listing is namesake-ambiguous is the same whichever listings
 *  are present.
 *
 *  Every rule that decided such a listing reads other listings -- a family majority's bound, the solver's tiers and
 *  lowest-id joins, the `venues.current` fill, sibling denials, shared posters, a learned veto -- and each is a count or a
 *  ratio over a set that grows and shrinks: UK "Hen" at Mowlem, Lighthouse Poole and Keswick, DE Torrington's "Dracula",
 *  CinemaxX's "Titans", Lichtblick's "Buddies" and Keswick's "These Violent Delights" took a namesake and lost it again as
 *  listings arrived. A namesake-ambiguous listing therefore takes NO film: its facts cannot break the tie, and nothing
 *  else may. */
private[identity] object NamesakeAmbiguity {

  /** The works `scored` (a node's own pool, scored) holds that its title could be: named by the title, of a kind no rule
   *  of the node's own rules out (the probability cut alone rules out nothing: it weighs `venues.corroborating`). */
  def plausible(scored: Seq[Scored]): Seq[Int] =
    scored.filter(s => (s.titleNamesIt || s.imdbTitled.nonEmpty) && s.untaken.isEmpty && (!s.denied || s.deniedByCutOnly))
      .map(_.candidate.tmdbId).distinct.sorted

  /** The node's namesakes (two plausible works or more) when its own facts single none out; else none. */
  def namesakes(node: EvidenceNode, scored: Seq[Scored]): Seq[Int] = {
    val works = plausible(scored)
    if (works.sizeIs >= 2 && singledOut(node, scored, works).isEmpty) works else Nil
  }

  /** Among `works`, the one the node's own facts single out, read in turn, each narrowing only where it leaves some:
   *   0. its title's HOUSE or season: the work its billing names under the venue's own stage house (an "NT Live:" banner
   *      names the National Theatre's staging, [[StageRelays]]), or a relay its screening days place in its record's season;
   *   1. the facts [[Corroboration.facts]] reads -- a year within one, a director it credits (or its text names whole), an
   *      original title of its own, a running time no namesake fits as well, a season, a cast -- corroborating it;
   *   2. its title STATES an original title: billed "Local (Original)" or "Local / Original", the works whose original
   *      title the second piece is, which the first piece does not title (PL "Lalka (Dolly)" is Stone's "Dolly");
   *   3. its venue's COUNTRY: the works TMDB dates a release of there, when every other is an OLDER work whose release
   *      dates are known and date none there (DE "Casablanca MASCHEN & MOVIES" is Curtiz's). A recent work may not be
   *      dated there yet, so it is never ruled out by it.
   *  A work whose record is not fetched is unknown to each, never ruled out by it. */
  def singledOut(node: EvidenceNode, scored: Seq[Scored], works: Seq[Int]): Option[Int] = {
    val pool = scored.filter(s => works.contains(s.candidate.tmdbId)).distinctBy(_.candidate.tmdbId)
    def narrowed(of: Seq[Scored], keep: Seq[Scored] => Seq[Scored]) = Option(keep(of)).filter(_.nonEmpty).getOrElse(of)
    val housed       = narrowed(pool, _.filter(s => s.houseProduction || s.seasonProduction))
    val corroborated = narrowed(housed, _.filter(s => Corroboration.facts(node, s, scored, _ => None).nonEmpty))
    val stated       = narrowed(corroborated, statesOriginal(pool.head.listing, _))
    val shown        = (node.country zip pool.head.listing.screeningsSeason).fold(stated) { case (c, season) => narrowed(stated, releasedIn(c, season, _)) }
    shown.map(_.candidate.tmdbId).distinct match {
      case Seq(only) => Some(only)
      case _         => None
    }
  }

  /** Does a fact of `listing`'s own, read apart from its node, decide it -- a pin (to a film, or with another listing) or
   *  a TMDB-keyed feed's link that decides on its own ([[SourceReliability.linkDecides]]; a copy's link needs a confirmation
   *  that only siblings gave: CinemaxX Raschplatz's "Titans" took both namesakes by turns as listings arrived)? Then it is no namesake-ambiguous listing: the cluster's rules for pins and links decide it. */
  def ownDecided(listing: Listing, pins: PinConstraints): Boolean =
    SourceReliability.tmdbLinked(listing).exists { case (id, _) => SourceReliability.linkDecides(id) } || pins.filmOf(listing.key).isDefined ||
      pins.pinnedTogether(listing.key)

  // "Local (Original)" or "Local / Original": the two titles a billing states, as keys -- a bracket the venue's
  // qualifiers fill ("(Napisy)", "(OmU)") names no work's original title, and so narrows nothing
  private val TwoTitles = """^(.+?)\s*(?:\((.+)\)|\s/\s(.+))\s*$""".r
  private[identity] def statedTitles(listing: IdentityMeasures.Listing): Seq[(String, String)] =
    listing.ownTitles.collect { case TwoTitles(local, bracketed, slashed) =>
      (IdentityMeasures.key(local), IdentityMeasures.key(Option(bracketed).getOrElse(slashed)))
    }.filter { case (local, original) => local.nonEmpty && original.nonEmpty && local != original }.distinct

  private def statesOriginal(listing: IdentityMeasures.Listing, works: Seq[Scored]): Seq[Scored] = {
    val stated = statedTitles(listing)
    works.filter { s =>
      val film = s.candidate.film
      film.originalTitle.map(IdentityMeasures.key).exists(original => stated.exists { case (local, second) =>
        second == original && !film.titleKeys.contains(local) })
    }
  }

  private def releasedIn(country: String, season: Int, works: Seq[Scored]): Seq[Scored] = {
    val (shown, unshown) = works.partition(_.candidate.film.knownReleasedIn(country, dated = true))
    def older(s: Scored) = s.candidate.film.year.exists(_ < season - 1)
    if (unshown.forall(s => older(s) && s.candidate.film.knownReleasedIn(country, dated = false))) shown else Nil
  }

  /** Why a namesake-ambiguous listing took no film. */
  def why(works: Seq[Int]): String =
    s"${DecisionTrace.Namesake}: its title names ${works.size} works (${works.take(5).mkString(", ")}) and no fact of its own tells them apart: no film"
}
