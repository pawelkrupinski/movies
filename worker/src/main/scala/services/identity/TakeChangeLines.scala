package services.identity

import models.Country
import settings.CommitSha

/**
 * The log lines of a projection's take changes ([[TakeChange]]): one per listing whose TMDB film moved, and a summary
 * of the tick. Each is a readable message — its `identity take change:` prefix kept for every query and alert that greps
 * for it — and the same facts as FIELDS, which the worker's JSON log lines carry into VictoriaLogs (`tools.JsonLineEncoder`)
 * so an investigation filters and groups on them instead of regexing the text:
 *
 *  - the move: `kind`, `venue`, `listing` (as billed), `film_before`/`film_after` (stored film ids), `tmdb_before`/
 *    `tmdb_after`, `title_before`/`title_after`;
 *  - why: `basis`, `confidence`, `reason_first`/`reason_last` (the explanation's ends), `rules` (the rule ids that decided
 *    the listing: `accept:`, `pooled:`, `veto:`, `join:`, `apart:`, `refused:`), `blocker` (what stopped a listing no rule
 *    took), `runner_up` (the candidate it beat, or the best it did not take), `fallback`, `unanswered`;
 *  - the take before: `previous_basis`, `previous_confidence`, `previous_reason` ([[LiveProjectionIndex.priorTake]]);
 *  - the listing's own facts: `listing_year`, `listing_runtime`, `listing_director`, `listing_original_title`;
 *  - `repro`: the `scripts/identity-slice` line that replays the listing under the build that took it.
 *
 * The country, the build (`build`) and the projection (`projection_id`) are every line's own fields already.
 */
final class TakeChangeLines(country: Country, build: CommitSha) {
  import TakeChangeLines.*

  /** The line of `take`: the stored film it was on titled `was`, the film it is on now (`now`: id and title), the
   *  decision that took it, the listing as published and the take before. */
  def of(take: TakeChange, was: Option[String], now: Option[(String, String)], decision: Option[ResolverDecision],
         listing: Option[Listing], prior: Option[TakeChange.Prior]): Line = {
    val key    = take.listing
    // The last explanation first: a stage that took or corrected the film after the resolver appends its own why.
    val reason = decision.fold("no decision")(d => (d.basis.toString +: (d.explanation.lastOption.toSeq ++ d.explanation.headOption).distinct).mkString(": "))
    val node   = decision.flatMap(_.trace.nodes.get(key))
    val repro  = reproOf(key)
    val message = s"identity take change: ${take.kind.label} ${key.venue} | ${key.rawTitle}: " +
      s"${film(take.before, was)} -> ${film(take.after, now.map(_._2))} (${take.previousFilm}); reason $reason" +
      prior.fold("")(p => s"; before ${p.basis} (${ResolverDecision.percent(p.confidence)})${p.reason.fold("")(r => s": $r")}") +
      s"; explain: $repro"
    Line(message, Seq(
      "kind"                   -> take.kind.label,
      "venue"                  -> key.venue,
      "listing"                -> key.rawTitle,
      "film_before"            -> take.previousFilm,
      "film_after"             -> now.map(_._1),
      "tmdb_before"            -> take.before,
      "tmdb_after"             -> take.after,
      "title_before"           -> was,
      "title_after"            -> now.map(_._2),
      "basis"                  -> decision.map(_.basis.toString),
      "confidence"             -> decision.map(d => rounded(d.confidence)),
      "reason_first"           -> decision.flatMap(_.explanation.headOption),
      "reason_last"            -> decision.flatMap(_.explanation.lastOption),
      "rules"                  -> decision.map(_.trace.rulesOf(key)).filter(_.nonEmpty).map(_.mkString(" ")),
      "blocker"                -> node.flatMap(_.blocker),
      "runner_up"              -> node.flatMap(_.candidates.headOption),
      "fallback"               -> decision.flatMap(_.fallback).map(f => s"${f.source}:${f.id}"),
      "unanswered"             -> decision.map(_.unanswered).filter(_ > 0),
      "previous_basis"         -> prior.map(_.basis.toString),
      "previous_confidence"    -> prior.map(p => rounded(p.confidence)),
      "previous_reason"        -> prior.flatMap(_.reason),
      "listing_year"           -> listing.flatMap(_.year),
      "listing_runtime"        -> listing.flatMap(_.runtime),
      "listing_director"       -> listing.map(_.directors).filter(_.nonEmpty).map(_.mkString("; ")),
      "listing_original_title" -> listing.flatMap(_.originalTitle),
      "repro"                  -> repro))
  }

  /** The tick's summary: how many takes moved by kind and by basis (`basisOf` each listing's, none when no decision
   *  names it), and how many of them were logged a line each. */
  def summary(takes: Seq[TakeChange], basisOf: TakeChange => Option[ResolverDecision.Basis], logged: Int): Line = {
    val byKind  = takes.groupMapReduce(_.kind.label)(_ => 1)(_ + _)
    val byBasis = takes.groupMapReduce(t => basisOf(t).fold("no decision")(_.toString))(_ => 1)(_ + _)
    def render(counts: Map[String, Int]) = counts.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")
    val unlogged = takes.size - logged
    Line(s"identity take change: summary ${takes.size} take changes (${render(byKind)}); by basis ${render(byBasis)}" +
      (if (unlogged > 0) s"; $unlogged more not logged" else ""),
      Seq("take_changes" -> takes.size, "logged" -> logged, "by_basis" -> render(byBasis)) ++
        TakeChange.Kind.values.toSeq.map(kind => kind.label -> byKind.getOrElse(kind.label, 0)))
  }

  /** The command that replays `key` — its venue and billed title, exactly — under the code of the build that took it:
   *  `scripts/identity-slice`, which decides a matched listing as well as an unmatched one, model and agreement. */
  private def reproOf(key: services.movies.ListingKey): String =
    s"scripts/identity-slice ${country.code} --key ${quoted(s"${key.venue}|${key.rawTitle}")}" +
      Option.when(build.value != "unknown")(s" --ref ${build.value}").getOrElse("")
}

object TakeChangeLines {
  /** A log line: its text and its fields (a `None` value leaves its field out). */
  final case class Line(message: String, fields: Seq[(String, Any)])

  private def film(tmdb: Option[Int], title: Option[String]) = tmdb.fold("none")(id => s"tmdb $id") + title.fold("")(t => s" ($t)")
  private def rounded(p: Double): Double = math.round(p * 1000) / 1000.0
  /** `text` as one single-quoted shell word. */
  private def quoted(text: String): String = "'" + text.replace("'", "'\\''") + "'"
}
