package services.identity.agreement

import scala.collection.mutable

import models.ListingKey
import services.identity.measures.Film
import services.identity.measures.Listing
import services.identity.vocabulary.{Answer, CandidateQuery, Dependency, DetailFacts, Hit, Question, VoterFamily}
import services.identity.{IdentityCalibration, IdentityLookups, ReadEvent}
import services.titles.TitleNormalizer

/**
 * The EVIDENCE the agreement stage decides on, and how it is read: the families' answers, the venues' pages and TMDB's
 * lookups — each read noted in the RECEIPT of the decision being made ([[receipting]], through [[Noting]]), and read
 * again now ([[reread]], [[digestNow]]) when a filing reaches a receipt that names it. An answer attempted and not given
 * is read by the settle rule here too ([[tried]], [[settled]]).
 *
 * The stage hands it to each of its parts — the candidates, the takes, the corrections — as the one way they read: none
 * of them reaches the stage itself. `version` and `yearNow` are the apply under way's, set by the stage as it starts one.
 */
private[agreement] final class AgreementEvidence(sources: AgreementSources.EvidenceSource, val calibration: IdentityCalibration, tmdbOf: String => Answer[Option[Int]],
                                                val attempted: Question => Boolean, val tmdbReads: TmdbReadTracking) {
  val families: Map[VoterFamily, FamilyAnswers] = sources.families
  val normalizer: TitleNormalizer               = sources.normalizer
  private val tmdb: Option[IdentityLookups]     = sources.tmdbLookups

  /** The evidence nodes the model's resolver reads of `listings` through `lookups` ([[IdentityResolver.evidenceOf]]):
   *  their TMDB candidates, each scored, under the stage's normalizer and calibration. */
  def nodesOf(listings: Seq[Listing], lookups: IdentityLookups): Seq[services.identity.IdentityResolver.NodeEvidence] =
    services.identity.IdentityResolver.evidenceOf(listings, lookups, normalizer, calibration)(_ => true)

  /** The families in their order: the order their verdicts and searches are read in. */
  val familiesInOrder: Seq[(VoterFamily, FamilyAnswers)] = families.toSeq.sortBy(_._1.ordinal)
  /** Each family's calibration: TMDB's, its search priors' spread scaled by the family's [[VoterFamily.priorSpread]]. */
  val calibrations: Map[VoterFamily, IdentityCalibration] = families.keys.map(family => family -> calibration.withPriorSpread(family.priorSpread)).toMap

  /** The venues' lookups: their pages, and nothing of TMDB's, whatever `venuePages` answers besides ([[AgreementEvidence.PagesOnly]]) —
   *  what the worker hands the stage there (VenueDetailLookups), so a harness handing one lookups for both roles reads
   *  as production reads. Handed TMDB's records through this role, every harness read the model's vote by its facts
   *  where production read it unknown, and none of them saw prod keep 26 takes a whole resolve made (2026-10-27). */
  val venues: IdentityLookups = AgreementEvidence.PagesOnly(sources.venuePages)

  /** `listing`'s venue detail page as a cluster's digest reads it ([[AgreementPipeline]]): none without one. */
  def detailMark(listing: Listing): String = if (venues.hasDetail(listing)) "\u0001" + venues.detail(listing) else ""
  /** The listing with the year, directors, running time, original title and countries its venue's own page states where
   *  the listing does not ([[services.identity.Evidence.of]], as the model reads it): what the agreement's listing votes
   *  and title check read, never what rules a film out ([[Agreement.agreed]]). PL "The Taxidermist | Splat!FilmFest" bills no director; its page credits Paulo Nascimento.
   *  Marked as holding its page's facts (`pageFacts`), which are a catalogue's claim where the page is a listings site's
   *  catalogue entry ([[services.identity.SourceReliability.listingsSiteEntry]]: Flicks). */
  def asStated(listing: Listing): Listing =
    if (!venues.hasDetail(listing)) listing
    else venues.detail(listing).answered.flatten.fold(listing)(listing.withPage)

  /** Is there a TMDB to read at all? A harness without one reads no poster and no candidate. */
  def hasTmdb: Boolean = tmdb.isDefined

  /** TMDB's and the venues' lookups as the stage reads them: each read noted ([[Noting]]). */
  lazy val looked: Option[IdentityLookups] = tmdb.map(new Noting(_, Dependency.Via.Tmdb))
  lazy val venuesLooked: IdentityLookups   = new Noting(venues, Dependency.Via.Venues)

  /** The version of the families' answers the apply under way runs at, and the year it reads a release as current by
   *  — an input every verdict's receipt names ([[Dependency.Year]]): set by the stage as it starts an apply. */
  var version = -1L
  var yearNow = -1

  /** The receipt the decision being made notes what it reads in ([[Noting]]): set by [[receipting]] on the applying
   *  thread. */
  private var receipt: mutable.Map[Dependency, Long] = null
  /** `decide`, everything it reads through the stage's lookups noted in `into`, its receipt. */
  def receipting[A](into: mutable.Map[Dependency, Long])(decide: => A): A = {
    val was = receipt
    receipt = into
    try decide finally receipt = was
  }
  /** `reads`, read on the way to the decision being made: in its receipt too, if one is being made. */
  def noted(reads: Iterable[(Dependency, Long)]): Unit = if (receipt != null) receipt ++= reads

  /** `inner`, each read noted: in the [[receipt]] being made, if any — as the question it reads, through `via`, with the
   *  digest of what it read, nothing there yet too ([[AgreementEvidence.reread]] reads it again) — and TMDB's
   *  records and searches among what the apply read ([[TmdbReadTracking]]). */
  private final class Noting(inner: IdentityLookups, via: Dependency.Via) extends services.identity.ForwardingIdentityLookups(inner) {
    private def noted[A](aspect: Dependency.Aspect, question: Question, answer: Answer[A]): Answer[A] = {
      if (receipt != null) receipt(Dependency.Lookup(via, aspect, question)) = Dependency.digest(answer)
      answer
    }
    private def record[A](aspect: Dependency.Aspect, tmdbId: Int, answer: Answer[A]): Answer[A] = { tmdbReads.record(tmdbId); noted(aspect, Question.FilmRecord(tmdbId), answer) }
    override def detail(listing: Listing): Answer[Option[DetailFacts]]               = noted(Dependency.Aspect.Answer, Question.VenuePage(listing.key), inner.detail(listing))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]]                 = { tmdbReads.queried(); noted(Dependency.Aspect.Answer, Question.Candidate(query), inner.candidates(query)) }
    override def film(tmdbId: Int): Answer[Option[Film]]            = record(Dependency.Aspect.Answer, tmdbId, inner.film(tmdbId))
    override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]]        = record(Dependency.Aspect.Release, tmdbId, inner.releaseDay(tmdbId))
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]]                      = record(Dependency.Aspect.Cast, tmdbId, inner.cast(tmdbId))
    override def overview(tmdbId: Int): Answer[Option[services.identity.vocabulary.ProseWords]] = record(Dependency.Aspect.Overview, tmdbId, inner.overview(tmdbId))
  }

  /** The digest of what `read` reads now ([[reread]]), of the listings `listingOf` names: the one digest a receipt
   *  records it by ([[Dependency.digest]]), so no kind is compared in another form than it was recorded in. */
  def digestNow(read: Dependency, listingOf: ListingKey => Option[Listing]): Option[Long] =
    reread(read, listingOf).map(Dependency.digest)
  /** What `read` — one no listing names: the year, an attempt — digests to now. A receipt records it by this, the reader it
   *  is compared through ([[digestNow]]): one form both ways. */
  def readNow(read: Dependency): Option[Long] = digestNow(read, _ => None)

  /** What a receipt's dependency reads now — the one reader a receipt is compared through: a family's answer, as
   *  [[AgreementEvidence.recording]] notes it; TMDB's and the venues' lookups', as [[Noting]] notes them, a detail page of
   *  a listing `listingOf` names; whether a question was attempted; the year the apply reads by. None for one nothing
   *  reads again — what holds it never stands. */
  def reread(read: Dependency, listingOf: ListingKey => Option[Listing]): Option[Answer[?]] = read match {
    case Dependency.Attempted(question)     => Some(Dependency.attemptRead(attempted(question)))
    case Dependency.Year                    => Some(Dependency.yearRead(yearNow))
    case Dependency.Lookup(Dependency.Via.Venues, aspect, question) => AgreementEvidence.reread(venues, aspect, question, listingOf)
    case Dependency.Lookup(Dependency.Via.Tmdb, aspect, question)   => tmdb.flatMap(AgreementEvidence.reread(_, aspect, question, listingOf))
    case Dependency.Answer(Question.Family(family, question)) => families.get(family).flatMap(familyAnswer(_, question))
    case Dependency.Asked(_) | Dependency.Unparsed(_) => None
  }

  /** `answers`' answer now to a family's `question`, as [[AgreementEvidence.recording]] names it (`title|…`, `director|…`,
   *  `record|…`, `showing|…`); None for a question named otherwise. */
  private def familyAnswer(answers: FamilyAnswers, question: String): Option[Answer[?]] =
    question.split("\\|", 2) match {
      case Array("title", text)    => Some(answers.titled(text))
      case Array("director", text) => Some(answers.directedBy(text))
      case Array("record", text)   => Some(answers.record(text))
      case Array("showing", text)  => Some(answers.showing(text))
      case _                       => None
    }

  /** Has the family answered `question` since — any answer, fresh? Read per waiting cluster, per tick: an answer filed
   *  for another cluster's question resolves none of the rest again (prod PL 2026-10-04: re-resolving every waiting
   *  cluster on each filing ran a 24 s, 540 MB agreement phase back to back). */
  def answered(gap: Question): Boolean = gap match {
    case Question.Family(family, question) => families.get(family).exists(_.fresh(question))
    case _                                 => false
  }

  /** Has `gap` been attempted: an answer to it held — a stale one too, read all the same — or, by `attempted`, asked and
   *  come to a durable non-answer or a failure? */
  def tried(gap: Question): Boolean = gap match {
    case family: Question.Family => answered(gap) || reread(Dependency.Answer(family), _ => None).exists(_ != Answer.Unknown) || attempted(gap)
    case _                                 => attempted(gap)
  }

  /** [[Agreement.settledBy]] this stage's `attempted`. */
  def settled[A](question: Question, answer: Answer[A]): Answer[A] = Agreement.settledBy(attempted)(question, answer)

  /** A family's answer a receipt holds that is stale, as the question to ask again. */
  def staleOf(read: Dependency): Option[Question] = read match {
    case Dependency.Answer(question @ Question.Family(family, asked)) if families.get(family).exists(!_.fresh(asked)) => Some(question)
    case _                                                                                                           => None
  }

  /** The families' verdicts the last applies resolved, each handed back while all it read reads alike ([[FamilyVerdicts]]). */
  val familyVerdicts = new FamilyVerdicts(venues, normalizer, calibrations)

  /** Every family's verdict on `listings`, in the families' order — each question read noted in `reads`, each gap in
   *  `gaps`. */
  def verdictsOf(listings: Seq[Listing], gaps: mutable.Set[Question], reads: mutable.Map[Dependency, Long]): Seq[Answer[FamilyVerdict]] =
    familiesInOrder.map { case (_, answers) => familyVerdicts.verdict(listings, answers, gaps, reads) }

  /** The TMDB film a record names: its TMDB id, else the film TMDB finds by its IMDb id — `Unknown` while that find is
   *  not answered yet, the IMDb id then noted in `finding`; `Failed` once it was attempted and not answered
   *  ([[Agreement.tmdbFound]]), which a reader decides on as naming none ([[Answer.failedAs]]). */
  def tmdbFilmOf(tmdbId: Option[Int], imdb: Option[String], finding: mutable.Set[Question]): Answer[Option[Int]] =
    tmdbId.fold(imdb.fold[Answer[Option[Int]]](Answer.Known(None)) { id =>
      tmdbReads.find(id)
      Agreement.tmdbFound(tmdbOf, id, finding, attempted)
    })(film => Answer.Known(Some(film)))
}

private[agreement] object AgreementEvidence {

  /** A model decision's cluster as its evidence is read: its key, its listings sorted, and their digest with TMDB's vote
   *  on it — what its verdict and take are read and kept by. */
  final case class Cluster(id: String, listings: Seq[Listing], digest: Long)
  /** The decision's listings, sorted by key. */
  def sortedListings(decision: services.identity.measures.ResolverDecision, listingOf: ListingKey => Option[Listing]): Seq[Listing] =
    decision.members.flatMap(listingOf).sortBy(_.key)(using ListingKey.ordering)
  /** One of `listings`, by its key: a receipt reads the detail page of its cluster's own listings alone. */
  def listingIn(listings: Seq[Listing]): ListingKey => Option[Listing] = key => listings.find(_.key == key)

  /** `inner`'s venue pages alone: TMDB's searches and records unknown, a film's cast and description none to go on —
   *  as the worker's venue lookups answer them (VenueDetailLookups). TMDB is the stage's `tmdb`. */
  final class PagesOnly private (val inner: IdentityLookups) extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                       = inner.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]]      = inner.detail(listing)
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit =
      inner.prefetch(Nil, Nil, details)
    override def prefetchAnswered(): Unit                          = inner.prefetchAnswered()
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]        = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[Film]]   = Answer.Unknown
  }
  object PagesOnly {
    def apply(lookups: IdentityLookups): IdentityLookups = lookups match {
      case pages: PagesOnly => pages
      case other            => new PagesOnly(other)
    }
  }

  /** `inner`, a listings site's catalogue entry linked as a listing's page read as no page at all ([[AgreementCandidates.uncatalogued]]). */
  final class CatalogueBlind(inner: IdentityLookups) extends services.identity.ForwardingIdentityLookups(inner) {
    private def entry(listing: Listing) = listing.page.exists(services.identity.measures.SourceReliability.listingsSiteEntry)
    override def hasDetail(listing: Listing): Boolean                  = !entry(listing) && inner.hasDetail(listing)
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = if (entry(listing)) Answer.Known(None) else inner.detail(listing)
  }

  /** `inner`, noting whether any answer it gave was `Unknown` — a cast, overview or release day not known left unnoted:
   *  it leaves the evidence that reads it unread, never the fill waiting. */
  final class UnknownNoting(inner: IdentityLookups) extends services.identity.ForwardingIdentityLookups(inner) {
    @volatile var unknown = false
    private def noted[A](answer: Answer[A]): Answer[A] = { if (answer == Answer.Unknown) unknown = true; answer }
    override def detail(listing: Listing): Answer[Option[DetailFacts]]    = noted(inner.detail(listing))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = noted(inner.candidates(query))
    override def film(tmdbId: Int): Answer[Option[Film]] = noted(inner.film(tmdbId))
  }

  /** The answer a lookup's dependency names (as [[AgreementEvidence]] notes it), read from `lookups` now: a detail
   *  page of a listing `listingOf` names (one it does not reads as moved). */
  def reread(lookups: IdentityLookups, aspect: Dependency.Aspect, question: Question, listingOf: ListingKey => Option[Listing]): Option[Answer[?]] =
    (question, aspect) match {
      case (Question.VenuePage(key), Dependency.Aspect.Answer)  => listingOf(key).map(lookups.detail)
      case (Question.Candidate(query), Dependency.Aspect.Answer) => Some(lookups.candidates(query))
      case (Question.FilmRecord(film), Dependency.Aspect.Answer)   => Some(lookups.film(film))
      case (Question.FilmRecord(film), Dependency.Aspect.Release)  => Some(lookups.releaseDay(film))
      case (Question.FilmRecord(film), Dependency.Aspect.Cast)     => Some(lookups.cast(film))
      case (Question.FilmRecord(film), Dependency.Aspect.Overview) => Some(lookups.overview(film))
      case _                                                       => None
    }

  /** A family's answers that note every question still a gap, and the digest of every answer read; `alsoOnRead` sees each
   *  read first. */
  private[agreement] def recording(answers: FamilyAnswers, asked: mutable.Set[Question], reads: mutable.Map[Dependency, Long],
                                   alsoOnRead: ReadEvent => Unit = _ => ()): FamilyAnswers =
    new RecordingAnswers(answers, {
      case event @ ReadEvent.FamilyAnswered(question, answer) =>
        alsoOnRead(event)
        noted(answers, question, Dependency.digest(answer), answer == Answer.Unknown, asked, reads)
      case event => alsoOnRead(event)
    })

  /** A family's answer to `question` read, as its digest and whether it was a gap: in the receipt `reads` — answered or
   *  not, so an answer filed where none was reads the verdict again as one that moved does — and among the questions
   *  `asked` when a gap, or read and stale (asked again). */
  private[agreement] def noted(answers: FamilyAnswers, question: String, digest: Long, unknown: Boolean, asked: mutable.Set[Question],
                               reads: mutable.Map[Dependency, Long]): Unit = {
    reads(Dependency.Answer(Question.Family(answers.family, question))) = digest
    if (unknown || !answers.fresh(question)) asked += Question.Family(answers.family, question)
  }
}
