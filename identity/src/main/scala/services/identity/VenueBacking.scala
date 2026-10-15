package services.identity

import services.identity.IdentityMeasures.*

/** Who backs a film from a title group: the listing's venue, and whose claim its facts are — the venue's own, or the feed
 *  entry they were copied from (`SourceReliability.claimOf`) — kept as one string per group entry. A venue's own claim
 *  is the venue's name itself, no new string per listing for the corpus's life; a feed's is `venue` and `source` joined
 *  by [[Backer.Join]]. */
object Backer {
  val Join = '\u0001'
  def apply(venue: String, source: String): String = if (source == venue) venue else s"$venue$Join$source"
  def venue(key: String): String  = { val at = key.indexOf(Join); if (at < 0) key else key.substring(0, at) }
  def source(key: String): String = { val at = key.indexOf(Join); if (at < 0) key else key.substring(at + 1) }
}

/** The venues backing a film for every asker of the same title groups, each group's backing venues
 *  of a film found once: a wide release lists one title at thousands of venues over one candidate
 *  pool, and asking per member re-reads the whole group per member. A group no venue lists (an
 *  undecorated title nobody lists bare) backs nothing. One per resolve or calibration pass; not
 *  thread-safe. */
final class VenueBacking(groups: Map[String, Seq[(String, Listing)]]) {
  // By the record INSTANCE, not its value: hashing a whole record (every title, credit and
  // country) per lookup cost more than the backing it saved. An equal record held twice is only
  // computed twice, to the same venues.
  private val memo = scala.collection.mutable.HashMap.empty[String, java.util.IdentityHashMap[Film, Set[String]]]
  private def backing(titleGroups: Seq[String], f: Film): Iterator[String] =
    titleGroups.iterator.flatMap(group => memo.getOrElseUpdate(group, new java.util.IdentityHashMap[Film, Set[String]]())
      .computeIfAbsent(f, film => VenueBacking.backers(film, groups.getOrElse(group, Nil))))
  /** The venues of `titleGroups` (a listing's [[titleGroups]]) other than `ownVenue` backing `f`. */
  def corroborating(titleGroups: Seq[String], f: Film, ownVenue: String): Int =
    (backing(titleGroups, f).map(Backer.venue).toSet - ownVenue).size
  /** The SOURCES of `titleGroups` other than `ownSource` backing `f` ([[Backer.source]]): a feed entry's facts, copied
   *  onto every venue it lists, are one claim, counted once — what a feed's own link is weighed against
   *  (`services.identity.FeedLink`). */
  def sources(titleGroups: Seq[String], f: Film, ownSource: String): Int =
    (backing(titleGroups, f).map(Backer.source).toSet - ownSource).size
}

object VenueBacking {
  /** Every [[Backer]] of `group` (the listings sharing the listing's title key) whose OWN facts back `f` — a title
   *  naming it, and its exact year or a credit of its director. */
  private def backers(f: Film, group: Seq[(String, Listing)]): Set[String] = {
    // A node's venues share one listing: its answer is found once, not once per venue.
    val backs = new java.util.IdentityHashMap[Listing, java.lang.Boolean]()
    group.iterator.filter { case (_, l) => backs.computeIfAbsent(l, listing => Boolean.box(backsFilm(listing, f))) }.map(_._1).toSet
  }
  /** Do the listing's own facts back `f`? The venue's title must NAME the film: a year or a director
   *  alone backs every film of that year or that director, and the walk of a director's filmography
   *  turns up all of them. Nor may another fact it publishes rule the film out ([[factsRuleOut]]). */
  private def backsFilm(l: Listing, f: Film): Boolean =
    namesFilm(l, f) && (
      l.statedYear.exists(y => f.year.contains(y)) ||
        creditsItsDirector(l, f)) && !factsRuleOut(l, f)

  /** Does a fact the listing publishes beside the year it shares with `f` rule the film out: another person credited in
   *  the same script, sharing no name's stem ([[FactRelations.otherPeople]]: "Marc Donskoi" is "Mark Donskoy") — a credit
   *  in another script is no other person here ("Wong Kar Wai" is TMDB's 王家衛) — or a runtime
   *  [[RuntimeContradiction]] minutes or more off? A year is every film of that year's: DE kinoheld's "MIRA" [2026]
   *  {Marie Limkilde} [85′] backed Daniella Saba's 16-minute 2026 "Mira", which then stood beside Limkilde's film for
   *  every bare "Mira" as corroborated, its release veto lifted. Never the runtime of a stage house's relay billed
   *  under the record's own house ([[StageRelays.billsItsHouse]]): the venue bills the performance with its intervals, UK "Royal
   *  Ballet: La Bayadère" [2018] [170′] the house's 128-minute recording (555004), and ruled out, its two venues backed
   *  nothing and it fell below the cut. */
  private def factsRuleOut(l: Listing, f: Film): Boolean =
    f.directors.exists(credits => FactRelations.otherPerson(l.directors, credits) && FactRelations.otherPeople(l.directors, credits)) ||
      (runtimeDelta(l, f, None) match {
        case Number(off) => math.abs(off) >= RuntimeContradiction && !(l.billsAStageHouse && StageRelays.billsItsHouse(l, f))
        case _           => false
      })
}
