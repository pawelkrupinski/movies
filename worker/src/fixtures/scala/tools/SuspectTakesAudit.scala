package tools

import services.identity.SuspectTakes
import services.identity.SuspectTakes.{Suspicion, Take}

/**
 * The suspect-take RATCHET over a whole corpus's takes ([[SuspectTakes]]): every take a signal suspects is judged in
 * the identity judgements ([[IdentityJudgements]]: right or wrong) or tolerated by them in the served gate — an
 * undecided suspect standing since the ratchet was armed, or a known-wrong take of scope `served` — and no take is one
 * they call wrong but those. A suspect neither judges is a NEW one: judge it (right, or wrong and fixed).
 *
 * Keyed by country, raw title and film: one judgement of a billing covers every venue billing it, as a tolerance does.
 */
object SuspectTakesAudit {

  /** A billing taken as `film` that a signal suspects: every venue billing it and every reason. */
  final case class Suspect(country: String, rawTitle: String, film: String, title: String, venues: Seq[String], reasons: Seq[Suspicion]) {
    /** As a tolerance keys it: a raw title single-spaced ([[UnmatchedClusters.singleSpaced]]). */
    def key: (String, String, String) = (country, clean(rawTitle), film)
    def signals: Seq[SuspectTakes.Signal] = reasons.map(_.signal).distinct
    def line: String = Seq(country, rawTitle, film, title, reasons.map(_.render).distinct.mkString("; ")).map(clean).mkString("\t")
    def render: String = s"$country\t$rawTitle\t$film $title\t(${venues.size} venue(s): ${venues.take(3).mkString(", ")}${if (venues.size > 3) ", …" else ""})" +
      s"\n    ${reasons.map(_.render).distinct.mkString("\n    ")}"
  }
  private def clean(text: String) = UnmatchedClusters.singleSpaced(text)

  /** A take as the labels judge it ([[UnmatchedClusters.verdict]]): its raw title single-spaced, as a TSV line holds it
   *  and a tolerance keys it. */
  def labelled(country: String, take: Take): UnmatchedClusters.Take =
    UnmatchedClusters.Take(country, take.listing.venue, clean(take.listing.rawTitle), take.tmdb, take.film.stripPrefix("imdb:") match {
      case id if take.film.startsWith("imdb:") => Some(id)
      case _ => Option.when(take.record.imdbNumber > 0)(f"tt${take.record.imdbNumber}%07d")
    }, take.decision.basis.toString, take.record.title, take.decision.agreed,
      Option.when(!take.film.startsWith("tmdb:") && !take.film.startsWith("imdb:"))(take.film))

  def verdict(country: String, take: Take, labels: Seq[UnmatchedClusters.Label]): Option[Boolean] =
    UnmatchedClusters.verdict(labelled(country, take), labels)

  /** Each billing's suspect take in `corpus`, its reasons over every venue billing it. */
  def suspects(country: String, corpus: SuspectTakes.Corpus, screeningYear: Int): Seq[Suspect] =
    SuspectTakes.suspected(corpus.takes, screeningYear, corpus.billings).flatMap { case (take, found) => found.map(take -> _) }.groupBy { case (t, _) => (t.listing.rawTitle, t.film) }
      .toSeq.map { case ((raw, film), found) =>
        val take = found.head._1
        Suspect(country, raw, film, s"${take.record.title} (${take.record.year.fold("?")(_.toString)})", found.map(_._1.listing.venue).distinct.sorted,
          found.map(_._2).distinct)
      }.sortBy(_.key)

  /** What the ratchet makes of a corpus's takes: the suspects no label judges and `tolerated` does not list (`fresh`),
   *  the billings taken as a film a label calls wrong that it does not list (`wrong`), and its billings no suspect or
   *  wrong take of this corpus is any more (`gone`: to strike, never a failure — a corpus moves nightly). */
  final case class Verdict(suspects: Seq[Suspect], fresh: Seq[Suspect], wrong: Seq[Take], gone: Seq[(String, String, String)]) {
    def failed: Boolean = fresh.nonEmpty || wrong.nonEmpty
  }

  private def key(country: String, take: Take) = (country, clean(take.listing.rawTitle), take.film)

  def judge(country: String, corpus: SuspectTakes.Corpus, screeningYear: Int, labels: Seq[UnmatchedClusters.Label],
            tolerated: Set[(String, String, String)]): Verdict = {
    val all       = suspects(country, corpus, screeningYear)
    val byTitle   = labels.filter(_.country == country).groupBy(label => clean(label.rawTitle))
    val verdicts  = corpus.takes.map(take => take -> byTitle.get(clean(take.listing.rawTitle)).flatMap(verdict(country, take, _)))
    val judged    = verdicts.collect { case (take, Some(_)) => key(country, take) }.toSet
    val wrongAll  = verdicts.collect { case (take, Some(false)) => take }.distinctBy(key(country, _))
    def keyed(line: (String, String, String)) = (line._1, clean(line._2), line._3)
    val listed    = tolerated.map(keyed)
    val fresh     = all.filterNot(s => judged(s.key) || listed(s.key))
    val wrong     = wrongAll.filterNot(take => listed(key(country, take)))
    val standing  = all.map(_.key).toSet ++ wrongAll.map(key(country, _))
    Verdict(all, fresh, wrong, tolerated.filter(line => line._1 == country && !standing(keyed(line))).toSeq.sorted)
  }

  /** A corpus's takes as a file the measure reads again (`scripts.SuspectTakesMeasure --dumps`, as
   *  `integration.SuspectTakesDump` writes it): every listing — a taken one as its venue's page states it — the
   *  decisions, and each taken film's record. What a leg leaves beside its verdict, so a signal is tuned on the very takes
   *  the leg judged. */
  def capture(country: models.Country, corpus: SuspectTakes.Corpus, decisions: Seq[services.identity.ResolverDecision],
              listings: Seq[services.identity.Listing]): UnmatchedClusters.Capture = {
    val stated = corpus.takes.map(t => t.listing.key -> t.listing).toMap
    UnmatchedClusters.Capture(country, listings.map(l => stated.getOrElse(l.key, l)), decisions, Map.empty,
      corpus.takes.flatMap(t => t.tmdb.map(_ -> Option(t.record))).toMap, Map.empty, Set.empty, Map.empty, Map.empty)
  }

  /** The reason a queue line gives a take a judgement calls wrong. */
  val WrongReason = "wrong by the judgements"

  /** A take a label calls wrong, as a line of the queue `scripts.SuspectTakesMeasure --out` writes. */
  def wrongLine(country: String, take: Take): String =
    Seq(country, take.listing.rawTitle, take.film, s"${take.record.title} (${take.record.year.fold("?")(_.toString)})", WrongReason)
      .map(clean).mkString("\t")

  /** How many of `suspects` each signal suspects: "fragment 3, era 1, …". */
  def byCount(suspects: Seq[Suspect]): String =
    SuspectTakes.Signal.values.map(s => s"${s.label} ${suspects.count(_.signals.contains(s))}").mkString(", ")

  /** The header of that queue: a suspect's line ([[Suspect.line]]) or a wrong take's ([[wrongLine]]). */
  val QueueHeader = "country\trawTitle\tfilm\ttitle\treasons"
}
