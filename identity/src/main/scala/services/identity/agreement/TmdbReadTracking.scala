package services.identity.agreement

import services.identity.vocabulary.Dependency

/**
 * What the agreement stage's takes read of TMDB afresh on every apply — its records, whether a search, the IMDb ids
 * found — outside any verdict's receipt: a filing of one must reach the stage as a filing a receipt read does
 * ([[AgreementStage.reads]]). The apply under way notes its reads ([[record]], [[find]], [[queried]]) from [[begin]] on;
 * [[publish]] makes them what [[reads]] answers by, until the next apply publishes its own.
 *
 * Concurrency: [[begin]], [[queried]] and [[publish]] run on the applying thread; [[record]] and [[find]] may be called
 * from the lookups the apply reads through, so the sets they fill are concurrent. [[reads]] is read from any thread (the
 * filing thread): the published sets are `@volatile` and never changed once published.
 */
final class TmdbReadTracking {
  private var recordsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[Integer]()
  private var findsReading   = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
  private var queriedReading = false
  @volatile private var recordsNow: java.util.Set[Integer] = java.util.Collections.emptySet()
  @volatile private var findsNow: java.util.Set[String]    = java.util.Collections.emptySet()
  @volatile private var queriedNow = false

  /** An apply starts: nothing read yet. */
  def begin(): Unit = {
    recordsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[Integer]()
    findsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    queriedReading = false
  }
  /** The apply read TMDB's record of `tmdbId`. */
  def record(tmdbId: Int): Unit = { val _ = recordsReading.add(tmdbId) }
  /** The apply read TMDB's find of the IMDb id `imdbId`. */
  def find(imdbId: String): Unit = { val _ = findsReading.add(AgreementStage.FindPrefix + imdbId) }
  /** The apply read a TMDB search. */
  def queried(): Unit = queriedReading = true

  /** What the apply read is what [[reads]] answers by from now. */
  def publish(): Unit = {
    recordsNow = recordsReading; findsNow = findsReading; queriedNow = queriedReading
  }

  /** Did the last published apply read what a filing under `id` comes as — a search ([[Dependency.QueryFiled]]), a find
   *  ([[AgreementStage.FindPrefix]]) or a record's day ([[AgreementStage.recordReadId]])? */
  def reads(id: String): Boolean = id match {
    case Dependency.QueryFiled => queriedNow
    case _ if id.startsWith(AgreementStage.FindPrefix) => findsNow.contains(id)
    case _ => AgreementStage.recordOf(id).exists(film => recordsNow.contains(film))
  }
}
