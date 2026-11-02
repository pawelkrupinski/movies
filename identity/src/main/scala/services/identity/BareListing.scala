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

  /** The bare node's film: the one plausible work, when its own alone rules took that work (`takenAlone`); else none. */
  def take(scored: Seq[Scored], takenAlone: Option[Int]): Option[Int] = plausible(scored) match {
    case Seq(only) if takenAlone.contains(only) => Some(only)
    case _                                       => None
  }

  /** Why a bare node took no film: its title's namesakes, or no work its own rules took. */
  def whyNone(scored: Seq[Scored]): String = plausible(scored) match {
    case Seq()      => s"${DecisionTrace.Bare}: no work its title names that its own evidence takes"
    case Seq(only)  => s"${DecisionTrace.Bare}: its own evidence does not take $only, the one work its title names"
    case namesakes  => s"${DecisionTrace.Bare}: its title names ${namesakes.size} works (${namesakes.take(5).mkString(", ")}): a near-tie, no film"
  }
}
