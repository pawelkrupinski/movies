package services.identity

/** A BARE listing's take, from its own title alone (docs/investigations/2026-11-01-flip-unification.md, §7.10; the owner,
 *  2026-11-03: "a take is a pure function of the current inputs, as if all data had been there from the start";
 *  "unmatched beats wrong; near-ties stay unmatched").
 *
 *  A node is BARE when nothing but its title speaks for it: it publishes no fact ([[IdentityMeasures]]'s
 *  `publishesAFact`: no year, season, runtime, director, country or original title of its own) and lists no cast, no pin
 *  names it, no feed links it to a film, and no catalogue id ties it to another node of its family. Its siblings then hold
 *  nothing about it but their own number and their own facts, and both change as listings arrive: a family's majority
 *  (`ClusterVoting.familyMajority`'s bound), the solver's join of the one cluster its title reaches, the `venues.current`
 *  fill, `Corroboration.refused`'s node counts. Each of those took DE CinemaxX's bare "Titans", UK Beehive's "The Addams
 *  Family", PL Światowid's "Billy Elliot" or Fishguard's "Romeo and Juliet" to a film and away again as listings
 *  arrived, with nothing departing and no answer moving.
 *
 *  So a bare node reads only what is the same whichever siblings are present: its own searches' candidates, scored
 *  against its own pool (a family scope of that node alone). It takes a film only when exactly ONE work of the pool is
 *  plausible for its title -- a candidate its title names (or IMDb lists under it), of the right kind (no rule of its own
 *  denies it but the probability cut, none leaves it untaken) -- and its own alone rules accept that work. TMDB holding
 *  namesakes is a near-tie its title cannot break: it takes no film, whatever its siblings' majority or venue count.
 *  The take is a function of the node alone, so it is the same on every arrival chain. */
private[identity] object BareListing {

  /** The works `scored` (a bare node's own pool, scored) holds that its title could be: named by the title, of a kind no
   *  rule of the node's own rules out. */
  def plausible(scored: Seq[Scored]): Seq[Int] =
    scored.filter(s => (s.titleNamesIt || s.imdbTitled.nonEmpty) && s.untaken.isEmpty && (!s.denied || s.deniedByCutOnly))
      .map(_.candidate.tmdbId).distinct.sorted

  /** The bare node's film: the one plausible work, when its own alone rules took that work (`takenAlone`); among several,
   *  the one its own facts single out ([[singledOut]]); else none. */
  def take(scored: Seq[Scored], takenAlone: Option[Int], country: Option[String]): Option[Int] = plausible(scored) match {
    case Seq(only)                 => Some(only).filter(takenAlone.contains)
    case namesakes if namesakes.nonEmpty => singledOut(scored.filter(s => namesakes.contains(s.candidate.tmdbId)), country)
    case _                         => None
  }

  /** Among a bare listing's NAMESAKES (several works its title names), the one its own facts and their records single out,
   *  read in turn, each narrowing only where it leaves some (the owner, 2026-11-03: decide from inputs that are the same
   *  whichever siblings are present; near-ties stay unmatched):
   *   1. its title STATES an original title: billed "Local (Original)" or "Local / Original", the works whose original
   *      title the second piece is, which the first piece does not title -- PL "Lalka (Dolly)" is Stone's "Dolly", not
   *      the Polish "Lalka" its first piece names;
   *   2. its venue's COUNTRY: the works TMDB dates a release of there, when every other is an OLDER work (from before the
   *      season before the one it screens in) whose release dates are known and date none there -- DE "Casablanca
   *      MASCHEN & MOVIES" is Curtiz's, the one of four namesakes Germany saw. A recent or upcoming work may not be dated
   *      there yet (UK "Royal Ballet & Opera: The Nutcracker" is the 2026/27 relay, not 2025/26's released one), so it is
   *      never ruled out by it; nor is the season's own release preferred to a classic (DE Delphi's "Possession" is
   *      Zulawski's 1981 film, not 2027's): the screening season dates no work.
   *  A work whose record is not fetched is unknown to each, never ruled out by it. */
  def singledOut(namesakes: Seq[Scored], country: Option[String]): Option[Int] = {
    val listing = namesakes.head.listing
    def narrowed(works: Seq[Scored], keep: Seq[Scored] => Seq[Scored]) = Option(keep(works)).filter(_.nonEmpty).getOrElse(works)
    val stated  = narrowed(namesakes, statesOriginal(listing, _))
    val shown   = (country zip listing.screeningsSeason).fold(stated) { case (c, season) => narrowed(stated, releasedIn(c, season, _)) }
    shown.map(_.candidate.tmdbId).distinct match {
      case Seq(only) => Some(only)
      case _         => None
    }
  }

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

  /** ANCHORED UNANIMITY (the owner, 2026-11-03: "can there be sibling dependence without flips?"): a bare listing its own
   *  facts leave undecided takes film X when its ANCHORS -- the siblings a title must-link joins it to, screening in its
   *  country and season, whose take is their OWN (a pin, a feed link, a fact or a cast of theirs, read on a scope of that
   *  sibling alone; never a vote, a borrowing or a fill) -- took X, and none took another film; and X is a work its title
   *  names, of a kind its own rules leave (`plausible`). No majority, venue count or lowest-id tie-break: an anchor's take
   *  is the same whichever listings are present, so as listings arrive the set of anchor films only grows, and the take
   *  is its one element or none -- none -> X, then X -> none once a conflicting anchor arrives (a real namesake screening
   *  beside it), never back. */
  def anchored(anchors: Set[Int], plausible: Set[Int]): Option[Int] =
    Option.when(anchors.sizeIs == 1)(anchors.head).filter(plausible)

  /** An ANCHOR's film by its own FACTS ([[anchored]]): the one work its title names, of a kind its own rules leave, that
   *  the facts it states agree with -- its year within one, its director the same person, its running time within ten
   *  minutes, each where the work's record holds it too, at least one of them read -- when a fact it states contradicts
   *  every other such work. A work whose record holds none of its facts is not ruled out: UK Pershore's bare "Royal
   *  Ballet: The Nutcracker" took the 2013 relay its sibling's running time agreed with, beside the 2026/27 one whose
   *  record held no running time yet. Read off the listing-film measures alone, never the calibrated
   *  probability: that weighs `venues.corroborating`, a count of the venues present, which moved the anchors of DE
   *  Cinecitta's "Camouflage" from one namesake to the other as listings arrived. */
  def factMatch(listing: IdentityMeasures.Listing, scored: Seq[Scored]): Option[Int] = {
    // each fact it states: Some(agrees) where the film's record holds it too, None where the record holds it not (unknown)
    def readings(s: Scored): Seq[Option[Boolean]] = Seq(
      Option.when(listing.year.nonEmpty)(s.number("year.delta").map(d => math.abs(d) <= 1)).flatten.map(Some(_)).getOrElse(None),
      Option.when(listing.directors.nonEmpty)(s.category("director").map(_ == IdentityMeasures.SamePersonMeasure.value)).flatten,
      Option.when(listing.runtime.nonEmpty)(s.number("runtime.delta").map(d => math.abs(d) <= 10)).flatten)
    def agrees(s: Scored) = { val read = readings(s).flatten; read.nonEmpty && read.forall(identity) }
    def contradicted(s: Scored) = readings(s).flatten.contains(false)
    val works = scored.filter(s => plausible(Seq(s)).nonEmpty)
    works.filter(agrees).map(_.candidate.tmdbId).distinct match {
      // every other work a fact it states contradicts: one whose record holds none of them is not ruled out
      case Seq(only) if works.forall(s => s.candidate.tmdbId == only || contradicted(s)) => Some(only)
      case _ => None
    }
  }

  /** Why a bare listing took what it took: nothing for its own take; its anchors' film, or why they gave none. */
  def why(scored: Seq[Scored], take: Option[Int], anchors: Set[Int]): Seq[String] = (take, anchors.toSeq.sorted) match {
    case (Some(film), Seq(only)) if film == only => Seq(s"${DecisionTrace.Bare}: its anchors, siblings taking a film on their own facts, all took $film")
    case (Some(_), _)                            => Nil
    case (None, several) if several.sizeIs > 1   => Seq(s"${DecisionTrace.Bare}: its anchors took ${several.mkString(", ")}: a namesake tie, no film")
    case (None, _)                               => Seq(whyNone(scored))
  }

  /** Why a bare node took no film: its title's namesakes, or no work its own rules took. */
  def whyNone(scored: Seq[Scored]): String = plausible(scored) match {
    case Seq()      => s"${DecisionTrace.Bare}: no work its title names that its own evidence takes"
    case Seq(only)  => s"${DecisionTrace.Bare}: its own evidence does not take $only, the one work its title names"
    case namesakes  => s"${DecisionTrace.Bare}: its title names ${namesakes.size} works (${namesakes.take(5).mkString(", ")}): a near-tie, no film"
  }
}
