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
   *   2. its venue's COUNTRY: the works TMDB dates a release of there, when every other's release dates are known and date
   *      none there -- DE "Casablanca MASCHEN & MOVIES" is Curtiz's, the one of four namesakes Germany saw;
   *   3. the SEASON it screens in: the work TMDB dates a cinema release of in that country in that season (or the next
   *      year), when every other's releases are known -- PL's bare "Lalka" in 2026 is the 2026 Lalka, not Has's 1968.
   *  A work whose record is not fetched is unknown to each, never ruled out by it. */
  def singledOut(namesakes: Seq[Scored], country: Option[String]): Option[Int] = {
    val listing = namesakes.head.listing
    def narrowed(works: Seq[Scored], keep: Seq[Scored] => Seq[Scored]) = Option(keep(works)).filter(_.nonEmpty).getOrElse(works)
    val stated  = narrowed(namesakes, statesOriginal(listing, _))
    val shown   = country.fold(stated)(c => narrowed(stated, releasedIn(c, _)))
    val current = (country zip listing.screeningsSeason).fold(shown) { case (c, season) => narrowed(shown, currentIn(c, season, _)) }
    current.map(_.candidate.tmdbId).distinct match {
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

  private def releasedIn(country: String, works: Seq[Scored]): Seq[Scored] = {
    val (shown, unshown) = works.partition(_.candidate.film.knownReleasedIn(country, dated = true))
    if (unshown.forall(_.candidate.film.knownReleasedIn(country, dated = false))) shown else Nil
  }

  private def currentIn(country: String, season: Int, works: Seq[Scored]): Seq[Scored] = {
    def cinema(s: Scored) = s.candidate.film.releases.map(TmdbFilmRecord.Release.all(_).filter(_.country == country))
    if (works.exists(cinema(_).isEmpty)) Nil
    else works.filter(cinema(_).exists(_.exists(r => r.year == season || r.year == season + 1)))
  }

  /** Why a bare node took no film: its title's namesakes, or no work its own rules took. */
  def whyNone(scored: Seq[Scored]): String = plausible(scored) match {
    case Seq()      => s"${DecisionTrace.Bare}: no work its title names that its own evidence takes"
    case Seq(only)  => s"${DecisionTrace.Bare}: its own evidence does not take $only, the one work its title names"
    case namesakes  => s"${DecisionTrace.Bare}: its title names ${namesakes.size} works (${namesakes.take(5).mkString(", ")}): a near-tie, no film"
  }
}
