package services.review

import models.Country
import services.identity.{ListingTrace, ResolverDecision}
import services.movies.ListingKey

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * One production listing explained (`/debug/review/listing`): its current take, where its take history is, the latest
 * decision trace, the judgements filed on it and its cluster on the review pages. Built from the reads the review
 * pages already make of the LOCAL prod mirror ([[ReviewSource]]) and the checkout's `identity-judgements.tsv`.
 *
 * The trace is the one production stored for the listing (`identity_traces`, rewritten with every re-resolve of its
 * family), never one kept here. Take HISTORY is stored nowhere the app reads: only production's
 * `identity take change:` log lines hold it, so the page hands over a ready LogsQL query ([[TakeHistory]]) instead of
 * the web app reading VictoriaLogs.
 *
 * @param decision the model decision its cluster holds, with the cluster as the review pages identify it
 * @param served   what prod serves the listing as: `Some(None)` served with no TMDB film, `None` no screening row
 */
final case class ListingExplain(country: Country, listing: ListingKey, decision: Option[(ResolverDecision, ReviewCluster)],
                                trace: Option[ListingTrace], served: Option[Option[Int]], films: Map[Int, FilmCard],
                                judgements: Seq[LabelRow]) {
  def history: TakeHistory = TakeHistory(country, listing)
  /** The take: the decision's film and basis, else the stored trace's (a listing whose family the mirror holds no
   *  decision for still has the trace production wrote beside it). */
  def film: Option[Int]   = decision.fold(trace.flatMap(_.film))(_._1.film)
  def basis: Option[String] = decision.map(_._1.basis.toString).orElse(trace.map(_.basis))
  /** The decision's film differs from the one production serves: the agreement stage or the projection took another. */
  def servedOtherwise: Boolean = served.exists(_ != film)
  /** The candidate the decision beat (a listing a rule took), or the best it did not take — first of the trace's. */
  def runnerUp: Option[String] = trace.flatMap(_.candidates.headOption)
  def title(tmdb: Int): String = films.get(tmdb).flatMap(_.title).fold(s"tmdb:$tmdb")(t => s"$t (tmdb:$tmdb)")
  /** The command that re-resolves this listing's cluster locally and explains it (`identity-slice --explain`). */
  def repro: String =
    s"scripts/identity-slice ${country.code} --explain --venue ${TakeHistory.quoted(listing.venue)} --title ${TakeHistory.quoted(listing.rawTitle)}"
}

object ListingExplain {
  /** The listings of `decisions` billed at `venue` as `title` exactly (several when one venue bills the same title on two
   *  pages); `None` venue matches every venue. */
  def find(decisions: Seq[ResolverDecision], venue: Option[String], title: String): Seq[ListingKey] =
    decisions.iterator.flatMap(_.members).filter(k => k.rawTitle == title && venue.forall(_ == k.venue)).toSeq.distinct

  /** For a title not billed exactly as asked: the listings whose raw title contains it, ignoring case — to pick from. */
  def near(decisions: Seq[ResolverDecision], venue: Option[String], title: String, limit: Int = 50): Seq[ListingKey] = {
    val wanted = title.toLowerCase(java.util.Locale.ROOT)
    decisions.iterator.flatMap(_.members).filter(k => k.rawTitle.toLowerCase(java.util.Locale.ROOT).contains(wanted) &&
      venue.forall(v => k.venue.toLowerCase(java.util.Locale.ROOT).contains(v.toLowerCase(java.util.Locale.ROOT))))
      .toSeq.distinct.sortBy(k => (k.rawTitle, k.venue)).take(limit)
  }

  /** `listing` explained from `source`'s reads and the judgement rows `labels`. */
  def build(country: Country, listing: ListingKey, source: ReviewSource, labels: Seq[LabelRow]): ListingExplain = {
    val serialised = ListingKey.serialised(listing)
    val decision   = source.decisions(unmatchedOnly = false).find(_.listings.contains(listing)).map(d => d -> ReviewCluster.of(country, d))
    val trace      = source.traces(Seq(serialised)).get(serialised)
    val served     = source.served().get(serialised)
    val ids = (decision.toSeq.flatMap { case (d, c) => d.film.toSeq ++ c.candidates.map(_.film) } ++ trace.flatMap(_.film) ++
      trace.flatMap(_.weighedFilm) ++ served.flatten).distinct
    val films = ReviewCards.withRecords(source.films(ids), source.filmRecords(ids))
    ListingExplain(country, listing, decision, trace, served, films, judgementsOf(country, listing, labels))
  }

  /** The rows judging `listing`: its country, its raw title (as the file normalises it), its venue or every venue's `*`. */
  def judgementsOf(country: Country, listing: ListingKey, labels: Seq[LabelRow]): Seq[LabelRow] =
    labels.filter(l => l.country == country.code && LabelsTsv.normalise(l.rawTitle) == LabelsTsv.normalise(listing.rawTitle) &&
      (l.venue == "*" || l.venue == listing.venue))

  /** This page's address for `listing`. */
  def path(country: Country, listing: ListingKey): String =
    s"/debug/review/listing?country=${country.code}&venue=${enc(listing.venue)}&title=${enc(listing.rawTitle)}"

  private[review] def enc(text: String): String = URLEncoder.encode(text, StandardCharsets.UTF_8)
}

/**
 * Where one listing's take history lives: production's `identity take change:` lines in VictoriaLogs (fields:
 * [[services.identity.TakeChangeLines]]), one per change — when, the build, the kind, the film before and after and why.
 * Lines before 2026-10-14 carry no fields (text only); [[legacyQuery]] finds those by their text.
 */
final case class TakeHistory(country: Country, listing: ListingKey) {
  import TakeHistory._
  /** Every take change of the listing in [[Window]], newest first, one row per change. */
  def query: String =
    s"""_time:$Window "identity take change" country:${country.code} venue:=${quotedLogs(listing.venue)} """ +
      s"""listing:=${quotedLogs(listing.rawTitle)} | sort by (_time desc) | fields ${Fields.mkString(", ")}"""
  /** The same changes found by their message alone, for lines older than the fields. */
  def legacyQuery: String =
    s"""_time:$Window "identity take change" country:${country.code} ${quotedLogs(s"${listing.venue} | ${listing.rawTitle}:")} | sort by (_time desc)"""
  /** The query open in VictoriaLogs' own UI (Google sign-in at [[LogsHost]]). */
  def link: String = uiLink(query)
  def legacyLink: String = uiLink(legacyQuery)
  /** `scripts/listing-history`, which adds the deploys and each change's replay. */
  def command: String =
    s"scripts/listing-history ${country.code} --venue ${quoted(listing.venue)} --title ${quoted(listing.rawTitle)} --since $Window"
}

object TakeHistory {
  val LogsHost = "https://logs.kinowo.net"
  /** VictoriaLogs keeps ~27 days. */
  val Window   = "30d"
  /** The columns a change shows: when, which build, what moved (before → after) and why — the decision's stage, rules,
   *  runner-up and the take before; what set it off (`trigger`), the stage and rule that decided it (`stage`, `decided_by`),
   *  its score against the runner-up's (`score`, `runner_up_*`, `margin`) and the title rules its title took. A field a
   *  line does not carry (every one logged before 2026-10-22 lacks these) is an empty cell. */
  val Fields: Seq[String] = Seq("_time", "build", "trigger", "kind", "tmdb_before", "title_before", "tmdb_after", "title_after",
    "basis", "confidence", "stage", "decided_by", "score", "runner_up_tmdb", "runner_up_score", "margin", "runner_up_won",
    "rules", "title_rules", "runner_up", "blocker", "reason_last", "previous_basis", "previous_decided_by", "repro")

  private def uiLink(query: String): String =
    s"$LogsHost/select/vmui/#/?query=${ListingExplain.enc(query).replace("+", "%20")}&g0.range_input=$Window"
  /** `text` as a LogsQL quoted phrase. */
  private[review] def quotedLogs(text: String): String = "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  /** `text` as one single-quoted shell word. */
  private[review] def quoted(text: String): String = "'" + text.replace("'", "'\\''") + "'"
}
