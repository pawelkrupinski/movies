package tools

/**
 * Divergences `HardClusterConvergenceIntegrationSpec` knows are REAL and NOT YET FIXED, so the
 * rest of its claim can guard everything else meanwhile. Here rather than in the spec so a unit
 * guard (`HardClusterRatchetSpec`) can hold them on every push, not only when itAll runs.
 *
 * Keyed by country code, then the film's FULL stored key (`sanitize|year`) — one film, not a
 * title: a title prefix exempted every year and every divergence of that title, so a new,
 * different break of the same film hid behind the old one.
 *
 * A ratchet, not an allowlist: an entry that no longer diverges FAILS the spec, so a fix has to
 * delete its entry. Never add one to get a build green — add the fix. Adding one also has to
 * raise [[MaxEntries]], which is the second, reviewable edit that says so.
 */
object HardClusterExemptions {

  /** Films whose SPLIT arrival settles differently from the reference permutation. */
  val SplitArrivalDivergences: Map[String, Set[String]] = Map.empty

  /** Films a further settle or an identical rescrape still rewrites. */
  val RescrapeChurn: Map[String, Set[String]] = Map.empty

  /** How many entries the two maps may hold together. Zero since the three the spec found on
   *  its first run were fixed (UK decorated-year rereleases, PL "Opętanie | klasyka w 4k", US
   *  "It"). Raising it is adding a known-broken film to the build's blind spot. */
  val MaxEntries = 0

  /** The hard-cluster seeds the corpus fixture holds a cluster for — the fixture only grows
   *  (`scripts/hard-clusters.sh` appends, never prunes), so the floor only rises. */
  val MinSeeds = 55

  /** Seeds the recorded corpus holds NO listing for — found when `HardClusterRatchetSpec` first
   *  looked (2026-09-24): the ratchet appended them from a failed leg's findings, but the
   *  corpus they were recorded against no longer carried those films. Replayed, they check
   *  nothing. Re-recording the corpus (scripts/hard-clusters.sh against a current archive) is
   *  the fix; the same ratchet — an entry that finds its cluster again fails the guard. */
  val SeedsWithoutCluster: Set[(String, String)] = Set(
    "uk" -> "Bring It On",
    "de" -> "Blood & Sinners",
    "de" -> "Die einfachen Dinge",
    "pl" -> "Robin Hood: Koniec legendy",
    "pl" -> "Głos Hind Rajab")

  def all: Seq[(String, String)] =
    (SplitArrivalDivergences.toSeq ++ RescrapeChurn.toSeq).flatMap { case (code, keys) => keys.toSeq.map(code -> _) }
}
