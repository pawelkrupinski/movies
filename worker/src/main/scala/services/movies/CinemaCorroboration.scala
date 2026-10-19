package services.movies

import models.{MovieRecord, Tmdb}
import services.resolution.{Candidate, Contradiction, Verdict}

/**
 * Does a row's own resolution survive contact with what its CINEMAS published?
 *
 * `resolveTmdbId` never re-runs once a `tmdbId` is set, so a wrong answer stands
 * for ever. Prod, 2026-09-05, carried five of them: "Vivaldi i ja" on an
 * 18-minute STABAT MATER concert short while 46 venues advertised 110 minutes and
 * named Damiano Michieletto; "Das Phantom der Oper" on the 1925 silent while ten
 * venues published 2004, 140 minutes and Joel Schumacher. Every one needed a hand
 * repair, and the evidence to catch every one was on the row the whole time.
 *
 * They arise because a deferred-detail cinema's FIRST scrape carries a title and
 * nothing else — year, director and runtime all arrive later with the detail — so
 * the first resolve is a title-only search, and `MovieCache.settleResolved` then
 * stamps that guess's year into the row's key, promoting it from guess to
 * identity. This is the other half of the loop: once the detail HAS landed, ask
 * again whether the cinemas agree.
 *
 * Both signals demand POSITIVE contradiction and abstain otherwise. A venue that
 * published nothing is not disagreeing, and read the other way this would
 * force-re-resolve the corpus.
 *
 * Detection is only the first of three steps: the since-deleted `CrewConfirmation` asked
 * TMDB who actually made the film before anything acted, and the since-deleted
 * `UnresolvedTmdbReaper` spent the re-resolution. Its scaladoc carried the warning that
 * mattered most: finding the RIGHT film can fail on its own, and a row left unresolved was
 * pruned from the read model rather than merely wrong. The operational log — how to re-measure the sweep, how
 * to audit its outcomes film-by-film, and which rows are deliberately left alone — is in
 * `docs/misresolution-sweep.md`.
 */
object CinemaCorroboration {

  /** Which signal contradicts, if either — [[services.resolution.Verdict]]'s rejection
   *  of the film the row's own `Tmdb` slot describes. The two reasons are not equally
   *  trustworthy, so the caller needs to know which fired: a runtime is a NUMBER the
   *  cinemas published and needs no confirming, while a director is a NAME, and a
   *  name can disagree for a dozen reasons that are not a different film. A caller
   *  about to spend a re-resolution on the director signal should confirm it first
   *  (the since-deleted `CrewConfirmation` asked TMDB's crew ids). */
  def contradiction(record: MovieRecord)(using LatestTitleYear): Option[Contradiction] =
    for {
      tmdbId <- record.tmdbId
      film   <- record.data.get(Tmdb)
      reason <- Verdict.of(record.evidence, Candidate.fromSlot(tmdbId, film)) match {
        case Verdict.Reject(r) => Some(r)
        case _                 => None
      }
    } yield reason

  /** True when the cinemas positively contradict the film this row resolved to.
   *  The cheap, PURE form — a corpus-scan metric uses it as-is; a caller about to
   *  act on it should go through [[contradiction]] and confirm a director. */
  def contradicts(record: MovieRecord)(using LatestTitleYear): Boolean = contradiction(record).isDefined
}
