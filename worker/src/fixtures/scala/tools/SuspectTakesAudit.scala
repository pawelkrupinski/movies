package tools

import services.identity.SuspectTakes
import services.identity.SuspectTakes.{Suspicion, Take}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * The suspect-take RATCHET over a whole corpus's takes ([[SuspectTakes]]): every take a signal suspects is judged in
 * `labels.tsv` ([[UnmatchedClusters.Label]]) or listed in the baseline of the suspects standing when the ratchet was
 * armed (`baseline.tsv`, which only ever shrinks), and no take is one a label calls wrong. A suspect neither judges is
 * a NEW one: judge it into `labels.tsv` (right, or wrong and fixed).
 *
 * Keyed by country, raw title and film: one judgement of a billing covers every venue billing it, as the baseline does.
 */
object SuspectTakesAudit {

  val Directory: Path = Path.of("test", "resources", "fixtures", "identity-suspects")
  val BaselinePath: Path = Directory.resolve("baseline.tsv")
  /** The lines the baseline holds (armed 2026-10-13 with 307; 80 once those were judged the same day; 75, then 112 on
   *  10-14 when a whole-corpus wrong-match hunt labelled 37 more served takes wrong, 110 once two of those proved right, and
   *  110 still when the sole-result coverage rule cured five and five UK takes the leg had never reached were listed; 111 with
   *  the UK leg's "Gremlins Special Event", taken as Innerspace by its director and running time alone): it only shrinks,
   *  so a lower count replaces this as it does. */
  val BaselineArmed = 111

  /** A billing taken as `film` that a signal suspects: every venue billing it and every reason. */
  final case class Suspect(country: String, rawTitle: String, film: String, title: String, venues: Seq[String], reasons: Seq[Suspicion]) {
    /** As the baseline spells it: a raw title's tabs and line breaks as spaces. */
    def key: (String, String, String) = (country, clean(rawTitle), film)
    def signals: Seq[SuspectTakes.Signal] = reasons.map(_.signal).distinct
    def line: String = Seq(country, rawTitle, film, title, reasons.map(_.render).distinct.mkString("; ")).map(clean).mkString("\t")
    def render: String = s"$country\t$rawTitle\t$film $title\t(${venues.size} venue(s): ${venues.take(3).mkString(", ")}${if (venues.size > 3) ", …" else ""})" +
      s"\n    ${reasons.map(_.render).distinct.mkString("\n    ")}"
  }
  private def clean(text: String) = text.replace('\t', ' ').replace('\n', ' ')

  /** A take as the labels judge it ([[UnmatchedClusters.verdict]]): its raw title spelled as a TSV line holds it, a line
   *  break as a space, as the baseline keys it. */
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

  /** What the ratchet makes of a corpus's takes: the suspects no label judges and the baseline does not list (`fresh`),
   *  the billings taken as a film a label calls wrong that the baseline does not list (`wrong`), and the baseline's lines
   *  no suspect or wrong take of this corpus is any more (`gone`: to strike from the baseline, never a failure — a corpus
   *  moves nightly). */
  final case class Verdict(suspects: Seq[Suspect], fresh: Seq[Suspect], wrong: Seq[Take], gone: Seq[(String, String, String)]) {
    def failed: Boolean = fresh.nonEmpty || wrong.nonEmpty
  }

  private def key(country: String, take: Take) = (country, clean(take.listing.rawTitle), take.film)

  def judge(country: String, corpus: SuspectTakes.Corpus, screeningYear: Int, labels: Seq[UnmatchedClusters.Label],
            baseline: Set[(String, String, String)]): Verdict = {
    val all       = suspects(country, corpus, screeningYear)
    val byTitle   = labels.filter(_.country == country).groupBy(_.rawTitle)
    val verdicts  = corpus.takes.map(take => take -> byTitle.get(clean(take.listing.rawTitle)).flatMap(verdict(country, take, _)))
    val judged    = verdicts.collect { case (take, Some(_)) => key(country, take) }.toSet
    val wrongAll  = verdicts.collect { case (take, Some(false)) => take }.distinctBy(key(country, _))
    val fresh     = all.filterNot(s => judged(s.key) || baseline(s.key))
    val wrong     = wrongAll.filterNot(take => baseline(key(country, take)))
    val standing  = all.map(_.key).toSet ++ wrongAll.map(key(country, _))
    Verdict(all, fresh, wrong, baseline.filter(_._1 == country).diff(standing).toSeq.sorted)
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

  /** The reason a baseline line gives a take a label calls wrong. */
  val WrongReason = "wrong by labels.tsv"

  /** A take a label calls wrong, as a baseline line. */
  def wrongLine(country: String, take: Take): String =
    Seq(country, take.listing.rawTitle, take.film, s"${take.record.title} (${take.record.year.fold("?")(_.toString)})", WrongReason)
      .map(clean).mkString("\t")

  /** How many of `suspects` each signal suspects: "fragment 3, era 1, …". */
  def byCount(suspects: Seq[Suspect]): String =
    SuspectTakes.Signal.values.map(s => s"${s.label} ${suspects.count(_.signals.contains(s))}").mkString(", ")

  def readBaseline(path: Path = BaselinePath): Set[(String, String, String)] =
    if (!Files.exists(path)) Set.empty
    else Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.drop(1).filter(_.nonEmpty).map(_.split("\t", -1))
      .map(a => (a(0), a(1), a(2))).toSet

  val BaselineHeader = "country\trawTitle\tfilm\ttitle\treasons"
}
