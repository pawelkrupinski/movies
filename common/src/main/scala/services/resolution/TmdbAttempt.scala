package services.resolution

import java.time.{Duration, Instant}

/**
 * The last TMDB resolution that concluded NO MATCH for a row — what it was tried
 * on, and when. Persisted on the row, beside the conclusion it explains.
 *
 * It replaces two things that could not agree: a `tmdbNoMatch` flag, which said a
 * search once found nothing but not what it searched with, and an in-memory
 * 24-hour negative cache, which said "don't ask again" to every event unless the
 * event itself carried a hint — so a director that arrived on the row rather than
 * on the event, or a Filmweb-supplied original title, sat unused until the TTL ran
 * out. Three re-open rules grew around that (the reaper's daily sweep, the merge
 * retrigger's "new disambiguator", the scrape path's carve-out) and disagreed.
 *
 * The one rule now: a miss is retried when the row's evidence and search terms
 * are no longer what the miss was reached on, or when the miss is older than
 * [[TmdbAttempt.RetryAfter]]. Nothing else asks.
 *
 * `evidence` is the fingerprint of [[FilmEvidence]] plus the derived search
 * terms (see [[TmdbAttempt.fingerprint]]); it identifies the inputs, not the row.
 */
final case class TmdbAttempt(evidence: String, at: Instant) {
  /** Does this attempt already answer for `evidence` as of `now`? */
  def covers(evidence: String, now: Instant): Boolean =
    this.evidence == evidence && !at.isBefore(now.minus(TmdbAttempt.RetryAfter))
}

object TmdbAttempt {
  /** How long a no-match stands before the same inputs are tried again — the
   *  cadence the negative cache and the daily reaper both used. */
  val RetryAfter: Duration = Duration.ofHours(24)

  /** A document written before attempts were recorded: it concluded no-match on
   *  inputs nobody wrote down, at a time nobody wrote down. Reads as "try again
   *  at the next look" — the fingerprint matches nothing and the time is older
   *  than any window. */
  val Legacy: TmdbAttempt = TmdbAttempt("", Instant.EPOCH)

  /** What a resolution attempt actually consumed: the cinemas' evidence and the
   *  extra search terms mined from derived slots (a Filmweb-supplied original
   *  title is a legitimate SEARCH input even though it is no evidence). Sorted
   *  and joined so two attempts on the same inputs fingerprint alike whatever
   *  order the inputs arrived in. */
  def fingerprint(evidence: FilmEvidence, searchTerms: Iterable[String]): String =
    tools.Digest.sha1Hex(evidence.canonical + "\u001d" + searchTerms.toSeq.distinct.sorted.mkString("\u001f"))

  def on(evidence: FilmEvidence, searchTerms: Iterable[String], at: Instant): TmdbAttempt =
    TmdbAttempt(fingerprint(evidence, searchTerms), at)
}
