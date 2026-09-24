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
   *  (`scripts/hard-clusters.sh` appends, never prunes), so the floor only rises.
   *
   *  53, not 55: two seeds were DROPPED on 2026-09-24, the one deliberate exception. The ratchet
   *  had appended them from a failed leg's findings, but no recorded corpus — the fixture, nor
   *  the newest full UK/DE recordings (run 35943410096) — holds a single listing of them, so
   *  they checked nothing: UK "Bring It On" and DE "Die einfachen Dinge", both off every screen
   *  by then. Three others missing from the fixture were re-extended from the full corpora. */
  val MinSeeds = 53

  def all: Seq[(String, String)] =
    (SplitArrivalDivergences.toSeq ++ RescrapeChurn.toSeq).flatMap { case (code, keys) => keys.toSeq.map(code -> _) }
}
