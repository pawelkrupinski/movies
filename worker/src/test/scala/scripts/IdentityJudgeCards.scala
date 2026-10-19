package scripts

import models.Country
import services.identity.{Answer, IdentityLookups, IdentityMeasures, SuspectTakes, TmdbIdentityLookups}
import services.movies.ListingKey
import tools.{IdentityJudgements, UnmatchedClusters}
import tools.UnmatchedClusters.{Capture, Label, Take}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Locale
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/**
 * An evidence card for every take of the unmatched-cluster capture no label judges (`scripts/identity-judge-cards`), so a
 * human or an agent judges it from one page instead of a venue page, TMDB and IMDb each:
 *
 *  - the LISTING side, from the capture: the billed title, the venues, the year, runtime and directors the billing and
 *    the venue page give, the venue's own catalogue ids, a synopsis snippet;
 *  - the RECORD side: the taken TMDB record's title, original title, year, runtime and directors — from the capture's
 *    answers first, then TMDB live (TMDB_API_KEY) for a record it never read;
 *  - why the take is suspect, when it is ([[SuspectTakes]]: a fragment, an era, a runtime, a sequel, a feed id, a weak
 *    fill route) — shown to the judge, never deciding;
 *  - a SUGGESTED verdict only where the evidence is decisive ([[JudgeEvidence.suggest]]), every other card left open.
 *
 * Writes `cards.md`, `labels-draft.tsv` (the decisive suggestions as identity-judgements.tsv lines, each noted "suggested … confirm")
 * and `open.tsv` (the rest, verdict blank) — never `identity-judgements.tsv` itself. `--all` cards the judged takes too and writes
 * `suggestions.tsv` beside the labels' verdict: how often a decisive suggestion agrees with a label.
 *
 * {{{
 *   IdentityJudgeCards [--fixtures dir] [--labels identity-judgements.tsv] [--country cc] [--out dir] [--all] [--offline]
 * }}}
 */
object IdentityJudgeCards {

  final case class Options(fixtures: Path = UnmatchedClusters.Directory, labels: Option[Path] = None, countries: Seq[String] = Nil,
                           out: Path = Path.of("target", "identity-judge"), all: Boolean = false, offline: Boolean = false)

  def parse(args: List[String], options: Options = Options()): Options = args match {
    case Nil                      => options
    case "--fixtures" :: v :: r   => parse(r, options.copy(fixtures = Path.of(v)))
    case "--labels" :: v :: r     => parse(r, options.copy(labels = Some(Path.of(v))))
    case "--country" :: v :: r    => parse(r, options.copy(countries = options.countries ++ v.split(",").map(_.trim.toLowerCase(Locale.ROOT))))
    case "--out" :: v :: r        => parse(r, options.copy(out = Path.of(v)))
    case "--all" :: r             => parse(r, options.copy(all = true))
    case "--offline" :: r         => parse(r, options.copy(offline = true))
    case other :: _               => sys.error(s"unknown argument $other")
  }

  /** One card: the takes of one billed title in one country that took one film. */
  final case class Card(country: String, rawTitle: String, film: String, takes: Seq[Take], listing: JudgeEvidence.Side,
                        record: Option[JudgeEvidence.Side], synopsis: Option[String], catalogue: Seq[String], labelled: Option[Boolean],
                        everyVenue: Boolean, suspicions: Seq[String] = Nil) {
    lazy val suggestion: JudgeEvidence.Suggestion = JudgeEvidence.suggest(listing, record, catalogue, takes.head.ids,
      broadcast = takes.head.basis == "Broadcast")
    def venues: Seq[String] = takes.map(_.venue).distinct.sorted
  }

  def main(args: Array[String]): Unit = {
    val options  = parse(args.toList)
    val started  = tools.Stopwatch.start()
    val labels   = IdentityJudgements.labels(options.labels.getOrElse(IdentityJudgements.CheckedIn))
    val captures = Country.all.filter(c => options.countries.isEmpty || options.countries.contains(c.code))
      .map(c => options.fixtures.resolve(s"${c.code}.json.gz")).filter(Files.exists(_)).map(UnmatchedClusters.read)
    val live     = if (options.offline) None else liveLookups()
    // the one wall-clock read: the day the drafts are noted with, and the screening year a dateless capture falls back to
    val today    = java.time.LocalDate.now(java.time.Clock.systemUTC())
    val cards    = captures.flatMap(capture => cardsOf(capture, labels, options.all, live, today))
    write(options.out, cards, options.all, today)
    val decisive = cards.filter(_.labelled.isEmpty).count(_.suggestion.verdict.isDefined)
    println(f"[identity-judge-cards] ${cards.count(_.labelled.isEmpty)} unjudged card(s) (${cards.filter(_.labelled.isEmpty).map(_.takes.size).sum} takes), " +
      f"$decisive with a decisive suggestion; written to ${options.out} in ${started.seconds}%.1fs")
    if (options.all) {
      val judged = cards.filter(c => c.labelled.isDefined && c.suggestion.verdict.isDefined)
      val wrong  = judged.filter(c => c.labelled != c.suggestion.verdict)
      println(s"[identity-judge-cards] against the labels: ${judged.size} judged card(s) with a decisive suggestion, ${wrong.size} disagree")
      wrong.foreach(c => println(s"  ${c.country}\t${c.rawTitle}\t${c.film}\tlabel ${verdictWord(c.labelled)}\tsuggested ${verdictWord(c.suggestion.verdict)}: ${c.suggestion.reasons.mkString("; ")}"))
    }
  }

  private def verdictWord(verdict: Option[Boolean]) = verdict.fold("open")(if (_) "right" else "wrong")

  /** TMDB live, for the records a capture never read: only with a key, and never asked twice for one film. */
  private def liveLookups(): Option[IdentityLookups] = settings.ProcessConfiguration.resolve().tmdbApiKey.map { key =>
    new TmdbIdentityLookups(new clients.TmdbClient(new tools.RealHttpFetch(), apiKey = Some(key), retrySleep = (ms: Long) => Thread.sleep(ms)),
      new services.enrichment.ImdbClient(new tools.RealHttpFetch()), Nil)
  }

  def cardsOf(capture: Capture, labels: Seq[Label], all: Boolean, live: Option[IdentityLookups], today: java.time.LocalDate): Seq[Card] = {
    val outcome  = UnmatchedClusters.replay(capture)
    val takes    = UnmatchedClusters.takes(capture, outcome).filter(take => all || UnmatchedClusters.verdict(take, labels).isEmpty)
    val byVenue  = capture.listings.groupBy(l => (l.venue, l.rawTitle))
    val takenBy  = UnmatchedClusters.takes(capture, outcome).groupBy(_.rawTitle).view.mapValues(_.map(_.film).toSet).toMap
    val groups   = takes.groupBy(take => (take.rawTitle, take.film)).toSeq.sortBy(_._1)
    // The records the capture never read, asked live on a small pool (TMDB allows ~40/s; 6 at once stays well inside).
    val tmdbOf   = (take: Take) => take.tmdb.orElse(take.imdb.flatMap(capture.finds.get).flatten)
    val missing  = groups.flatMap(g => tmdbOf(g._2.head)).distinct.filterNot(id => capture.films.get(id).flatten.isDefined)
    val fetched  = live.fold(Map.empty[Int, IdentityMeasures.Film])(lookups => fetchAll(missing, lookups))
    val suspect  = suspicionsOf(capture, outcome, today.getYear)
    groups.map { case ((raw, film), group) =>
      val listings = group.flatMap(take => byVenue.getOrElse((take.venue, take.rawTitle), Nil)).distinct
      val details  = listings.flatMap(l => capture.details.get(ListingKey.serialised(l.key)).flatten)
      val listing  = JudgeEvidence.Side(
        title     = raw,
        original  = JudgeEvidence.mostCommon(listings.flatMap(_.originalTitle) ++ details.flatMap(_.originalTitle)),
        year      = JudgeEvidence.mostCommon(listings.flatMap(_.year) ++ details.flatMap(_.year)),
        runtime   = JudgeEvidence.mostCommon(listings.flatMap(_.runtime) ++ details.flatMap(_.runtime)),
        directors = (listings.flatMap(_.directors) ++ details.flatMap(_.directors)).distinct)
      val record = tmdbOf(group.head).flatMap(id => capture.films.get(id).flatten.orElse(fetched.get(id))).map { f =>
        JudgeEvidence.Side(f.title, f.originalTitle.filterNot(_ == f.title), f.year, f.runtime, f.directors.getOrElse(Nil))
      }
      Card(capture.country.code, raw, film, group, listing, record, details.flatMap(_.synopsis).headOption,
        listings.flatMap(_.catalogueIds).map(c => s"${c.source}:${c.id}").distinct.sorted,
        if (all) UnmatchedClusters.verdict(group.head, labels) else None,
        everyVenue = takenBy.get(raw).contains(Set(film)),
        suspicions = group.flatMap(take => suspect.getOrElse((take.venue, take.rawTitle, take.film), Nil)).distinct)
    }
  }

  /** Why each take is suspect ([[SuspectTakes]], as the worker's audit reads the live resolve), by venue, billed title and
   *  film; the year it screens in, where a listing states no days, is the capture's latest screening year, else `thisYear`. */
  def suspicionsOf(capture: Capture, outcome: UnmatchedClusters.Outcome, thisYear: Int): Map[(String, String, String), Seq[String]] = {
    val corpus = SuspectTakes.corpus(outcome.agreed, capture.listings, new UnmatchedClusters.Replay(capture), id => capture.films.get(id).flatten)
    val year   = capture.listings.flatMap(_.screenings.first).map(_.getYear).maxOption.getOrElse(thisYear)
    SuspectTakes.suspected(corpus.takes, year, corpus.billings).collect { case (take, found) if found.nonEmpty =>
      (take.listing.venue, take.listing.rawTitle, take.film) -> found.map(_.render)
    }.toMap
  }

  private def fetchAll(ids: Seq[Int], lookups: IdentityLookups): Map[Int, IdentityMeasures.Film] = {
    val pool = Executors.newFixedThreadPool(6)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try {
      val started = tools.Stopwatch.start()
      val got = ParallelTasks.all(ids)(id => id -> lookups.film(id)).collect {
        case (id, Answer.Known(Some(film))) => id -> film
      }.toMap
      if (ids.nonEmpty) println(f"[identity-judge-cards] TMDB live: ${got.size}/${ids.size} record(s) in ${started.seconds}%.1fs")
      got
    } finally pool.shutdown()
  }

  // ── the files ────────────────────────────────────────────────────────────────────────────

  def write(out: Path, cards: Seq[Card], all: Boolean, today: java.time.LocalDate): Unit = {
    Files.createDirectories(out)
    // identity-judgements.tsv rows: a confirmed one is pasted in as it stands
    def labelLines(card: Card, verdict: String, note: String): Seq[String] =
      (if (card.everyVenue) Seq("*") else card.venues).map(venue => Seq(card.country, venue, IdentityJudgements.normalise(card.rawTitle), card.film,
        verdict, "open", "", card.record.fold("")(_.line), "", today.toString, note).map(_.replace('\t', ' ')).mkString("\t"))
    val open      = cards.filter(_.labelled.isEmpty)
    val draft     = open.filter(_.suggestion.verdict.isDefined).flatMap { card =>
      labelLines(card, verdictWord(card.suggestion.verdict), s"suggested $today by identity-judge-cards: ${card.suggestion.reasons.mkString("; ")} — CONFIRM")
    }
    val undecided = open.filter(_.suggestion.verdict.isEmpty).flatMap(card => labelLines(card, "",
      (card.record.fold("no record")(r => r.line) +: card.suspicions.map(s => s"suspect — $s")).mkString("; ")))
    val header    = IdentityJudgements.Columns.mkString("\t")
    Files.writeString(out.resolve("labels-draft.tsv"), (header +: draft).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    Files.writeString(out.resolve("open.tsv"), (header +: undecided).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    Files.writeString(out.resolve("cards.md"), open.sortBy(c => (c.suggestion.verdict.isEmpty, c.country, c.rawTitle)).map(markdown).mkString(
      s"# Evidence cards: ${open.size} unjudged take group(s)\n\nSuggestions are DRAFTS: confirm each before copying a line of labels-draft.tsv into identity-judgements.tsv.\n\n", "\n", ""),
      StandardCharsets.UTF_8)
    if (all) Files.writeString(out.resolve("suggestions.tsv"), ("country\trawTitle\tfilm\ttakes\tlabel\tsuggested\treasons" +: cards.map(c =>
      Seq(c.country, c.rawTitle, c.film, c.takes.size.toString, c.labelled.fold("")(v => verdictWord(Some(v))), c.suggestion.verdict.fold("")(v => verdictWord(Some(v))),
        c.suggestion.reasons.mkString("; ")).mkString("\t"))).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
  }

  def markdown(card: Card): String = {
    val take = card.takes.head
    val record = card.record
    def cell(value: Option[Any]) = value.fold("—")(_.toString)
    def people(names: Seq[String]) = if (names.isEmpty) "—" else names.mkString(", ")
    val venues = card.venues
    s"""## ${card.country} · «${card.rawTitle}» → ${card.film} ${take.title}
       |
       |**${card.suggestion.verdict.fold("OPEN")(if (_) "suggest RIGHT" else "suggest WRONG")}**${if (card.suggestion.reasons.nonEmpty) s" — ${card.suggestion.reasons.mkString("; ")}" else ""}
       |
       |${card.takes.size} take(s) at ${if (venues.size <= 4) venues.mkString(", ") else s"${venues.take(3).mkString(", ")} and ${venues.size - 3} more"}; basis ${take.basis}; ids ${take.ids.toSeq.sorted.mkString(", ")}
       |
       || | listing | record |
       ||---|---|---|
       || title | ${card.listing.title} | ${cell(record.map(_.title))} |
       || original title | ${cell(card.listing.original)} | ${cell(record.flatMap(_.original))} |
       || year | ${cell(card.listing.year)} | ${cell(record.flatMap(_.year))} |
       || runtime | ${cell(card.listing.runtime)} | ${cell(record.flatMap(_.runtime))} |
       || directors | ${people(card.listing.directors)} | ${people(record.toSeq.flatMap(_.directors))} |
       |
       |${if (card.suspicions.nonEmpty) card.suspicions.map(s => s"suspect — $s").mkString("", "  \n", "\n\n") else ""}${if (card.catalogue.nonEmpty) s"venue's catalogue ids: ${card.catalogue.mkString(", ")}\n\n" else ""}${card.synopsis.fold("")(s => s"> ${s.take(300).replace('\n', ' ')}\n\n")}why taken: ${take.explanation.lastOption.getOrElse("—").take(400)}
       |""".stripMargin
  }
}

/** The decisive-evidence rules a judge card suggests a verdict by — deliberately few: a card any rule leaves in doubt,
 *  or two rules pull apart, stays OPEN for a human. */
object JudgeEvidence {

  /** One side's facts: the listing's (billing and venue page) or the record's. */
  final case class Side(title: String, original: Option[String], year: Option[Int], runtime: Option[Int], directors: Seq[String]) {
    def line: String = s"'$title'${original.fold("")(o => s" / '$o'")} (${year.getOrElse("?")}, ${runtime.fold("?")(r => s"$r min")}" +
      s"${if (directors.nonEmpty) s", ${directors.mkString(", ")}" else ""})"
  }

  final case class Suggestion(verdict: Option[Boolean], reasons: Seq[String])

  /** A record running longer than the venue's whole slot by more than this share of it is another film (an extended
   *  cut is ~25% longer). The other way round decides nothing: an event's slot holds a talk, an album played, a
   *  festival's block (UK Purbeck Film Festival, 10-13: a 150-min slot for a 77-min film the poster names). */
  val RuntimeOff = 0.30
  /** Under this many minutes a record is a short; a listing this long or longer is a feature. */
  val Short = 40
  val Feature = 60

  def mostCommon[A](values: Seq[A]): Option[A] = values.groupBy(identity).toSeq.sortBy(-_._2.size).headOption.map(_._1)

  /** A director's surname, case and accents aside: "Peter Jackson" and "JACKSON, Peter" are one. */
  def surname(name: String): String = {
    val plain = java.text.Normalizer.normalize(name.toLowerCase(Locale.ROOT), java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "").replace("ł", "l")
    val ordered = plain.split(",", 2) match { case Array(last, first) => s"${first.trim} ${last.trim}"; case _ => plain }
    ordered.split("[^\\p{L}]+").filter(_.nonEmpty).lastOption.getOrElse("")
  }

  /** The verdict the evidence decides, if it does:
   *   - WRONG on a contradiction the record cannot explain: the venue's own catalogue id names another film of the same
   *     database; a record running more than [[RuntimeOff]] longer than the venue's slot, or a short billed as a
   *     feature; or both years and directors disagreeing.
   *   - A record dated two years or more after the year billed only LEANS wrong (a reason, no verdict): a stage relay
   *     bills its production's year (the Met's 2018 staging) and its record dates the season's relay (2027), a
   *     sing-along bills its film's (DE "Pettersson und Findus Mitmachkino 2": 2024 billed, the 2026 event right).
   *   - RIGHT where the venue's catalogue id names this film, or the years agree (±1) AND a director is shared, with no
   *     contradiction.
   *   - OPEN otherwise, and whenever a RIGHT and a WRONG reason meet. */
  def suggest(listing: Side, record: Option[Side], catalogue: Seq[String], ids: Set[String], broadcast: Boolean = false): Suggestion = {
    val against = Seq.newBuilder[String]
    val for_    = Seq.newBuilder[String]
    val leans   = Seq.newBuilder[String]
    val databases = ids.map(_.takeWhile(_ != ':'))
    catalogue.foreach { id =>
      if (ids(id)) for_ += s"the venue's catalogue id $id names it"
      else if (databases(id.takeWhile(_ != ':'))) against += s"the venue's catalogue id $id names another film"
    }
    record.foreach { r =>
      (listing.runtime, r.runtime) match {
        case (Some(l), Some(t)) if l > 0 && t > 0 =>
          if (math.min(l, t) < Short && math.max(l, t) >= Feature) against += s"a short against a feature: runtime $l vs $t min"
          else if (t > l && (t - l).toDouble / t > RuntimeOff) against += s"the record runs $t min, the venue's slot $l (>${(RuntimeOff * 100).toInt}% shorter)"
        case _ => ()
      }
      val shared = listing.directors.map(surname).toSet.intersect(r.directors.map(surname).toSet).filter(_.nonEmpty)
      val bothDirected = listing.directors.nonEmpty && r.directors.nonEmpty
      (listing.year, r.year) match {
        case (Some(l), Some(t)) if t >= l + 2 && !broadcast => leans += s"leans wrong: the record ($t) is dated after the year billed ($l)"
        case (Some(l), Some(t)) if math.abs(l - t) <= 1 && shared.nonEmpty => for_ += s"year $l ~ $t and director ${shared.mkString(", ")} shared"
        case (Some(l), Some(t)) if math.abs(l - t) > 1 && bothDirected && shared.isEmpty =>
          against += s"year $l vs $t and directors ${listing.directors.mkString(", ")} vs ${r.directors.mkString(", ")}"
        case _ => ()
      }
    }
    (for_.result(), against.result()) match {
      case (Nil, Nil)       => Suggestion(None, (if (record.isEmpty) Seq("no record to compare") else Nil) ++ leans.result())
      case (yes, Nil)       => Suggestion(Some(true), yes)
      case (Nil, no)        => Suggestion(Some(false), no)
      case (yes, no)        => Suggestion(None, (yes ++ no).map(r => s"conflict: $r"))
    }
  }
}
