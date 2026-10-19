package tools

import models.{Country, Source}
import org.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}
import org.bson.json.{JsonMode, JsonWriterSettings}
import services.identity._
import services.identity.adapters.{CatalogueAnswerStore, FamilyAnswerStore, InMemoryTmdbDocuments, PosterAnswerStore, TmdbKind}
import services.identity.agreement.{AgreementStage, FamilyAnswers, InMemoryAgreementVerdicts, VoterFamily}
import services.movies.{ListingKey, TitleNormalizer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.jdk.CollectionConverters._

/**
 * The clusters the identity model leaves UNMATCHED on a recorded full corpus, captured with every answer the resolver
 * and the agreement stage read for them — TMDB's (candidate searches, film records, venue detail pages, IMDb-id finds),
 * the other film database families' (`identity_family_answers` documents, as [[FamilyAnswerStore]] files them) and the
 * venue and TMDB posters' hashes (filed beside them by [[PosterAnswerStore]]) —
 * so the resolver plus the agreement over them is a pure function of one checked-in file per country
 * (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`). `integration.UnmatchedClustersCapture`
 * records it from the recorded corpora and the signal-combination experiment's answer cache (real answers, never
 * hand-written); `UnmatchedClustersRatchetSpec` replays it: zero wrong takes, and the right ones only ever grow.
 */
object UnmatchedClusters {

  val Directory: Path = Path.of("test", "resources", "fixtures", "identity-unmatched")
  def fixturePath(country: Country): Path = Directory.resolve(s"${country.code}.json.gz")

  /** The venue film pages whose catalogue links the catalogue take reads, as the worker declares them. */
  val CataloguePages: Seq[CatalogueLinkPages] =
    Seq(services.cinemas.common.FlicksClient.CatalogueLinkPages, services.cinemas.pl.KinotekaClient.CatalogueLinkPages)

  /** A country's unmatched clusters — the model's decisions on them over the whole corpus, and their listings — and
   *  every answer the agreement stage read for them. The decisions are the whole corpus's, never a re-resolve of the
   *  clusters alone: alone, a cluster loses the neighbours that hold it apart from a namesake (a Met relay alone took
   *  the Royal Opera's record of the same opera). */
  final case class Capture(country: Country, listings: Seq[Listing], decisions: Seq[ResolverDecision], queries: Map[CandidateQuery, Seq[Hit]],
                           films: Map[Int, Option[IdentityMeasures.Film]], details: Map[String, Option[DetailFacts]],
                           withDetail: Set[String], families: Map[String, BsonDocument], finds: Map[String, Option[Int]],
                           /** Each candidate's top-billed cast the fill read ([[IdentityLookups.cast]]): none in a capture
                            *  recorded before the cast evidence, whose replay knows no cast. */
                           casts: Map[Int, Option[Seq[String]]] = Map.empty,
                           /** The family questions the capture asked and never got answered ([[AgreementQuestionRounds]]: a
                            *  host that blackholed every attempt) — held no answer, so the replay leaves them open as
                            *  production leaves a failed fetch's, and the next capture asks them again. */
                           unreachable: Set[(VoterFamily, String)] = Set.empty,
                           /** Every TMDB and IMDb HTTP answer the capture's whole resolve read LIVE where its recording had a
                            *  gap (`IdentityShadow.LiveGapLeaf.answered`), keyed `GET <url>` / `POST <url> <body>` as the
                            *  replay asks it (the stub TMDB key): the answers a captured MODEL decision read that neither the
                            *  recording nor the agreement's own reads above keep — an IMDb suggestion, most often. A slice
                            *  replays them before its recording's gap ([[FiledAnswers.LiveLeaf]]). None in older captures. */
                           live: Map[String, String] = Map.empty,
                           /** The words each candidate's TMDB overviews capitalise, as the synopsis evidence read them
                            *  ([[IdentityLookups.overview]]): none in a capture recorded before it, whose replay knows none. */
                           overviews: Map[Int, Option[ProseWords]] = Map.empty)

  /** `capture` with what a recording's TMDB tree answers that it was taken without: the cast of every candidate the
   *  evidence asked one of (`asked`, beside the ones it already noted) and holds none for, and each held record's original
   *  language and genres. A capture taken through a [[MemoLookups]] that passed no cast on (before 5faaac6c8) noted every
   *  cast it asked as unknown, and a record parsed before the store kept `original_language` or `genres` holds none:
   *  answered off the tree, as the store's re-read record would be. What the tree states none of stays none; neither
   *  changes a model decision the capture holds. */
  def withRecorded(capture: Capture, asked: Iterable[Int], castOf: Int => Option[Seq[String]],
                   recordOf: Int => Option[IdentityMeasures.Film]): Capture = {
    val casts = (capture.casts.keys ++ asked).toSeq.distinct.filter(id => capture.casts.get(id).flatten.isEmpty)
      .flatMap(id => castOf(id).map(cast => id -> Option(cast)))
    val films = capture.films.collect { case (id, Some(film)) if film.originalLanguage.isEmpty || film.genreBits == 0 =>
      recordOf(id).map(recorded => id -> Option(film.copy(originalLanguage = film.originalLanguage.orElse(recorded.originalLanguage),
        genreBits = if (film.genreBits != 0) film.genreBits else recorded.genreBits)))
    }.flatten.filter { case (id, film) => !capture.films.get(id).contains(film) }
    capture.copy(casts = capture.casts ++ casts, films = capture.films ++ films)
  }

  /** The family questions `wanted` that `capture` holds no reason to leave open: those it gave up on as unreachable aside,
   *  every question the stage asks of a complete capture is answered. */
  def unasked(capture: Capture, wanted: Set[(VoterFamily, String)]): Set[(VoterFamily, String)] = wanted -- capture.unreachable

  /** The hosts (of the listings' pages) that `capture` marks as having venue detail (`withDetail`) and holds the detail
   *  of none of, with how many listings it marks: a host that refused the corpus recording's every detail read — the UK
   *  capture of 2026-10-12 over recording 38103789067, whose Cineworld box-office API reads were all 403'd: 384 listings
   *  with detail, none stored, read by the agreement as no venue facts at all (77 "RBO … Macbeth" right takes lost). A
   *  capture holding one is not to be checked in: re-record the corpus once the host answers again. A host counts once it
   *  marks [[RefusedHostListings]] listings: a small house's one page left unread is no refusal (PL, run 38315368375:
   *  Kino Marzenie's single listing). */
  def hostsMissingDetail(capture: Capture): Map[String, Int] = {
    val hostOf = capture.listings.flatMap(l => l.page.flatMap(page => scala.util.Try(java.net.URI.create(page).getHost).toOption.flatMap(Option(_)))
      .map(ListingKey.serialised(l.key) -> _)).toMap
    capture.withDetail.toSeq.flatMap(key => hostOf.get(key).map(_ -> key)).groupBy(_._1).collect {
      case (host, marked) if marked.size >= RefusedHostListings && marked.forall { case (_, key) => capture.details.get(key).flatten.isEmpty } =>
        host -> marked.size
    }
  }

  /** The listings a host marks before [[hostsMissingDetail]] reads none of its detail stored as the recording refused. */
  val RefusedHostListings = 10

  // ── what the agreement makes of the clusters ─────────────────────────────────────────────

  /** The model's no-match decisions, and what the agreement stage made of them — and every TMDB question the stage asked
   *  that the capture holds no answer to (`unanswered`: a fill rule's own resolve of a cluster asks searches the whole
   *  corpus's never did, and finds no evidence where the fixture is stale; `missingQueries` and `missingFilms` are those
   *  questions as asked, for a measure to answer live). */
  final case class Outcome(model: Resolution, agreed: Resolution, stage: AgreementStage, unanswered: Set[String] = Set.empty,
                           missingQueries: Set[CandidateQuery] = Set.empty, missingFilms: Set[Int] = Set.empty)

  /** The families each country asks, as the worker wires them (`IdentityProjectionWiring.agreementFamilies`). */
  def familiesOf(country: Country): Seq[VoterFamily] =
    Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(modules.wiring.IdentityProjectionWiring.FilmwebVoting(country.code))(VoterFamily.Filmweb)

  /** The model's decisions as a resolution the agreement stage is handed. */
  def resolutionOf(decisions: Seq[ResolverDecision]): Resolution =
    Resolution(decisions, decisions.map(_.members.size).sum, decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap,
      Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  /** The agreement stage over the model's decisions, as the worker runs it on the way to the projection. */
  def agree(decisions: Seq[ResolverDecision], listings: Seq[Listing], lookups: IdentityLookups, families: Map[VoterFamily, FamilyAnswers],
            version: Long, tmdbOf: String => Answer[Option[Int]], normalizer: TitleNormalizer, posters: PosterAnswers,
            identities: Seq[VoterFamily], catalogue: CatalogueAnswers, listedOn: Option[VoterFamily] = None,
            calibration: IdentityCalibration = IdentityCalibration.resolver): Outcome = {
    val model = resolutionOf(decisions)
    val stage = new AgreementStage(families, lookups, normalizer, calibration, tmdbOf, new InMemoryAgreementVerdicts,
      clock = SpecClock.Pinned, posters = posters, tmdb = Some(lookups), identities = identities, catalogue = catalogue, listedOn = listedOn)
    val byKey = listings.map(l => l.key -> l).toMap
    Outcome(model, stage.apply(model, byKey.get, version), stage)
  }

  /** The capture replayed: `Unknown` for anything it does not hold — a stale fixture, re-captured, never guessed. Over
   *  `decisions` (the capture's, else a part of them), each family's and the posters' answers read through `families` and
   *  `posters` — what a measure of the stage's questions wraps them in — normalised by `normalizer` (the country's
   *  title rules, else a candidate rule set a measure of it resolves with), and weighed by `calibration` (the shipped
   *  artefact, else a refit's proposal — `scripts.IdentityRefit`). */
  def replay(capture: Capture, decisions: Seq[ResolverDecision] = Nil, families: FamilyAnswers => FamilyAnswers = identity,
             posters: PosterAnswers => PosterAnswers = identity, normalizer: Option[TitleNormalizer] = None,
             calibration: IdentityCalibration = IdentityCalibration.resolver): Outcome = {
    val docs = new InMemoryTmdbDocuments
    docs.put(TmdbKind.Family, capture.families.toSeq)
    val store = new FamilyAnswerStore(docs, SpecClock.Pinned)
    val lookups = new Replay(capture)
    agree(if (decisions.isEmpty) capture.decisions else decisions, capture.listings, lookups,
      familiesOf(capture.country).map(f => f -> families(store.answers(f))).toMap, version = 1,
      imdb => capture.finds.get(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(Answer.Known(_)), normalizer.getOrElse(TitleNormalizer.forCountry(capture.country)),
      posters(new PosterAnswerStore(store, SpecClock.Pinned)), modules.wiring.IdentityProjectionWiring.identities(capture.country.code),
      new CatalogueAnswerStore(store, SpecClock.Pinned, CataloguePages), modules.wiring.IdentityProjectionWiring.listedOn(capture.country.code), calibration)
      .copy(unanswered = lookups.unanswered, missingQueries = lookups.queries.asScala.toSet, missingFilms = lookups.films.asScala.toSet.map(_.toInt))
  }

  // ── the capture's gaps ─────────────────────────────────────────────────────────────────────

  /** A lookup the replay asked that the capture holds no answer to — `kind` the seam (model, family, imdb-find, poster,
   *  catalogue, record, tmdb) and the lookup as rendered. */
  final case class Gap(country: String, kind: String, lookup: String)

  /** Every gap of `capture`'s replay `outcome`: the ratchet's re-capture questions, in one list. A replay reads each as
   *  `Unknown` — the leg's reading of a recording gap, and identity-slice's UNANSWERED — so the takes it makes stand. */
  def gaps(capture: Capture, outcome: Outcome): Seq[Gap] = {
    val cc = capture.country.code
    def all(kind: String, lookups: Iterable[String]) = lookups.toSeq.sorted.map(Gap(cc, kind, _))
    all("model", Seq(outcome.model.unknownQueries -> "queries", outcome.model.unknownFilms -> "films")
        .collect { case (n, what) if n > 0 => s"$n $what unanswered" }) ++
      // a question the capture gave up on as unreachable is left open as production leaves a failed fetch's
      all("family", unasked(capture, outcome.stage.wanted).map { case (family, key) => s"($family, $key)" }) ++
      all("imdb-find", outcome.stage.wantedFinds) ++
      all("poster", outcome.stage.wantedPosters.map(_.toString)) ++
      all("catalogue", outcome.stage.wantedCatalogue.map(_.toString)) ++
      all("record", outcome.stage.wantedRecords.map(id => s"tmdb $id")) ++
      // TMDB's answers the stage's own resolves ask — a fill rule's, a poster's candidates — that the capture never held
      all("tmdb", outcome.unanswered)
  }

  /** The gaps the ratchet read as unknown, as TSV (`scripts.UnmatchedClustersRatchet --measure`, how the takes check
   *  MEASURES a change over a capture lacking the lookups it newly asks: `capture-gaps.tsv`) — the takes it still writes,
   *  where it otherwise fails the capture as stale and every country's takes are lost to one country's gap. */
  def writeGaps(path: Path, gaps: Seq[Gap]): Unit = {
    Files.createDirectories(path.toAbsolutePath.getParent)
    Files.writeString(path, "country\tkind\tlookup\n" + gaps.map(gap => s"${gap.country}\t${gap.kind}\t${gap.lookup}\n").mkString,
      StandardCharsets.UTF_8)
  }

  /** One listing's take: the film its cluster took (TMDB's, or a fallback film), the IMDb id it is known by, and the
   *  fallback film's id where another database's stands for it ("filmweb:10105049", "wikidata:Q141180912"). */
  final case class Take(country: String, venue: String, rawTitle: String, tmdb: Option[Int], imdb: Option[String], basis: String, title: String,
                        agreed: Map[String, String] = Map.empty, standsOn: Option[String] = None,
                        /** The decision's explanation — the rule or fill that took the film, the guards it passed, its signals. */
                        explanation: Seq[String] = Nil) {
    /** Every id it is known by: TMDB's, IMDb's, the one it stands on, and each agreeing family's own ("filmweb:880000",
     *  "wikidata:Q1"). */
    def ids: Set[String] = tmdb.map(id => s"tmdb:$id").toSet ++ imdb.map(id => s"imdb:$id") ++ standsOn ++
      agreed.map { case (family, id) => s"${if (family == "wiki") "wikidata" else family}:$id" }
    def film: String = tmdb.fold(imdb.fold(standsOn.getOrElse(""))(id => s"imdb:$id"))(id => s"tmdb:$id")
  }

  private val NamedImdb = """ (tt\d+)$""".r.unanchored

  /** Every listing whose cluster took a film, in a stable order. */
  def takes(capture: Capture, outcome: Outcome): Seq[Take] = {
    val byKey = capture.listings.map(l => l.key -> l).toMap
    outcome.agreed.decisions.filter(d => d.film.isDefined || d.fallback.isDefined).flatMap { d =>
      val record = d.film.flatMap(id => outcome.agreed.films.get(id).orElse(capture.films.get(id).flatten))
      val imdb = d.fallback.filter(_.source == "imdb").map(_.id)
        .orElse(record.filter(_.imdbNumber > 0).map(f => f"tt${f.imdbNumber}%07d"))
        .orElse(d.explanation.lastOption.collect { case NamedImdb(id) => id })
      val title = record.map(f => s"${f.title} (${f.year.getOrElse("?")})").getOrElse(d.explanation.lastOption.getOrElse(""))
      val standsOn = d.fallback.filter(_.source != "imdb").map(taken => s"${taken.source}:${taken.id}")
      d.members.flatMap(byKey.get).map(l => Take(capture.country.code, l.venue, l.rawTitle, d.film, imdb, d.basis.toString, title, d.agreed, standsOn,
        d.explanation))
    }.sortBy(t => (t.country, t.venue, t.rawTitle))
  }

  // ── the ratchet's findings, machine-readable ─────────────────────────────────────────────

  /** One line of the ratchet's report: a take as the labels judge it — `WRONG`, `UNJUDGED`, `NEW` (right, not yet in
   *  `expected-matches.tsv`) or `RIGHT` — or an expected right take no longer taken: `MOVED` when its listing now takes
   *  another film (`film`, the old one in `was`), `LOST` when it takes none. */
  final case class Finding(status: String, country: String, venue: String, rawTitle: String, film: String, title: String, basis: String,
                           ids: Seq[String], was: String, explanation: Seq[String])

  val FindingOrder: Seq[String] = Seq("WRONG", "UNJUDGED", "MOVED", "LOST", "NEW", "RIGHT")

  /** Every take's finding, and every expected right take's that is gone — what a reviewer of a recapture works through. */
  def findings(takes: Seq[Take], labels: Seq[Label], expected: Set[(String, String, String, String)]): Seq[Finding] = {
    def line(take: Take) = (take.country, take.venue, take.rawTitle, take.film)
    def of(status: String, take: Take, was: String = "") =
      Finding(status, take.country, take.venue, take.rawTitle, take.film, take.title, take.basis, take.ids.toSeq.sorted, was, take.explanation)
    val now = takes.map(take => take -> verdict(take, labels)).map {
      case (take, Some(false)) => of("WRONG", take)
      case (take, None)        => of("UNJUDGED", take)
      case (take, _)           => of(if (expected(line(take))) "RIGHT" else "NEW", take)
    }
    val right  = takes.filter(take => verdict(take, labels).contains(true)).map(line).toSet
    val byLine = takes.groupBy(take => (take.country, take.venue, take.rawTitle))
    val gone = (expected -- right).toSeq.sorted.map { case (cc, venue, raw, film) =>
      byLine.getOrElse((cc, venue, raw), Nil).find(_.film != film) match {
        case Some(take) => of("MOVED", take, was = film)
        case None       => Finding("LOST", cc, venue, raw, "", "", "", Nil, film, Nil)
      }
    }
    (now ++ gone).sortBy(f => (FindingOrder.indexOf(f.status), f.country, f.venue, f.rawTitle))
  }

  /** Where the ratchet writes them. */
  val FindingsPath: Path = Path.of("target", "identity-unmatched", "ratchet-takes.tsv")

  val FindingsHeader: String = "status\tcountry\tvenue\trawTitle\tfilm\ttitle\tbasis\tids\twas\texplanation"

  /** The findings as TSV: one line each, tabs and line breaks inside a field flattened, the explanation's lines joined
   *  by " ¶ " — `scripts/identity-ratchet-report` and `scripts/identity-judge-cards` read it. */
  def writeFindings(path: Path, findings: Seq[Finding]): Unit = {
    def cell(text: String) = text.replace('\t', ' ').replace('\n', ' ').replace('\r', ' ')
    val lines = findings.map(f => Seq(f.status, f.country, f.venue, f.rawTitle, f.film, f.title, f.basis, f.ids.mkString(","), f.was,
      f.explanation.mkString(" ¶ ")).map(cell).mkString("\t"))
    Files.createDirectories(path.getParent)
    Files.writeString(path, (FindingsHeader +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
  }

  // ── labels: what each listing is, and is not ─────────────────────────────────────────────

  /** A judgement of one listing (`venue` "*" for every venue billing `rawTitle`): `film` ("tmdb:913760",
   *  "imdb:tt16315948") is `right` or `wrong` for it — a row of [[IdentityJudgements]] judging it. */
  final case class Label(country: String, venue: String, rawTitle: String, film: String, right: Boolean, note: String) {
    def covers(take: Take): Boolean =
      take.country == country && singleSpaced(take.rawTitle) == singleSpaced(rawTitle) && (venue == "*" || venue == take.venue)
  }

  /** A billing as every judgement keys it ([[IdentityJudgements.normalise]]). */
  def singleSpaced(raw: String): String = IdentityJudgements.normalise(raw)

  /** A take's verdict by the labels: `Some(true)` right, `Some(false)` wrong — a label denies its film, or the labels
   *  name its listing's right film in a database the take has an id in and this is not it — `None` unjudged. The take's
   *  own venue's label calling a film right overrules every venue's ("*") calling it wrong: PL Kino Iluzjon's "Lalka" is
   *  Has' 1968 film, the bare "Lalka" everywhere else Kawalski's 2026 one. */
  def verdict(take: Take, labels: Seq[Label]): Option[Boolean] = {
    val covering  = labels.filter(_.covers(take))
    val rightHere = covering.filter(label => label.venue != "*" && label.right).map(_.film).toSet
    val about     = covering.filterNot(label => label.venue == "*" && !label.right && rightHere(label.film))
    val databases = take.ids.map(_.takeWhile(_ != ':'))
    if (about.exists(l => !l.right && take.ids(l.film))) Some(false)
    else if (about.exists(l => l.right && take.ids(l.film))) Some(true)
    else if (about.exists(l => l.right && databases(l.film.takeWhile(_ != ':')))) Some(false)
    else None
  }

  def takeLine(take: Take): String = Seq(take.country, take.venue, take.rawTitle, take.film, take.title).mkString("\t")
  def readTakeLines(path: Path): Set[(String, String, String, String)] =
    if (!Files.exists(path)) Set.empty
    else Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.drop(1).filter(_.nonEmpty).map(_.split("\t", -1)).map(a => (a(0), a(1), a(2), a(3))).toSet

  // ── the known-wrong takes: the WRONG takes no safe fix removes, a list that only shrinks ──

  /** The wrong takes `known` does not list (`fresh`: each fails the ratchet), and its billings — (country, raw title,
   *  film), every venue billing it: the judgements' known-wrong rows of [[IdentityJudgements.Scope.Capture]] — no wrong
   *  take is any more (`stale`: each fails too, to strike — the fixture is checked in, so a cured row is cured for good). */
  final case class WrongAudit(fresh: Seq[Take], stale: Seq[(String, String, String)])

  def auditWrong(wrong: Seq[Take], known: Set[(String, String, String)]): WrongAudit = {
    def key(country: String, raw: String, film: String) = (country, singleSpaced(raw), film)
    val listed = known.map(key.tupled)
    val taken  = wrong.map(take => key(take.country, take.rawTitle, take.film)).toSet
    WrongAudit(wrong.filterNot(take => listed(key(take.country, take.rawTitle, take.film))),
      known.filterNot(line => taken(key.tupled(line))).toSeq.sorted)
  }

  // ── recording and replaying the lookups ──────────────────────────────────────────────────

  /** `inner`, every answer it gave noted — what a capture writes. */
  final class Recording(inner: IdentityLookups,
                        /** What answers the films' overviews ([[IdentityLookups.overview]]): `inner` unless it is a
                         *  [[MemoLookups]], which, dropped into refs older than the overview, passes none through. */
                        described: Option[IdentityLookups] = None) extends IdentityLookups {
    val queries    = new ConcurrentHashMap[CandidateQuery, Seq[Hit]]()
    val films      = new ConcurrentHashMap[Int, Option[IdentityMeasures.Film]]()
    val details    = new ConcurrentHashMap[String, Option[DetailFacts]]()
    val withDetail = ConcurrentHashMap.newKeySet[String]()
    val casts      = new ConcurrentHashMap[Int, Option[Seq[String]]]()
    val overviews  = new ConcurrentHashMap[Int, Option[ProseWords]]()
    override def hasDetail(listing: Listing): Boolean = {
      val has = inner.hasDetail(listing)
      if (has) withDetail.add(ListingKey.serialised(listing.key))
      has
    }
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = inner.prefetch(queries, films, details)
    override def prefetchAnswered(): Unit = inner.prefetchAnswered()
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = noted(inner.detail(listing))(details.put(ListingKey.serialised(listing.key), _))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = noted(inner.candidates(query))(queries.put(query, _))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId))(films.put(tmdbId, _))
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = noted(inner.cast(tmdbId))(casts.put(tmdbId, _))
    override def overview(tmdbId: Int): Answer[Option[ProseWords]] = noted(described.getOrElse(inner).overview(tmdbId))(overviews.put(tmdbId, _))
    private def noted[A](answer: Answer[A])(keep: A => Any): Answer[A] = { answer.toOption.foreach(keep); answer }
  }

  /** The captured answers as the resolver's lookups, every TMDB question it holds no answer to noted — `queries` and
   *  `films`, named in `unanswered` — to be asked live (`integration.UnmatchedClustersFill`). A venue page not read
   *  is no TMDB question: only a re-capture reads it. */
  final class Replay(capture: Capture) extends IdentityLookups {
    val queries = ConcurrentHashMap.newKeySet[CandidateQuery]()
    val films   = ConcurrentHashMap.newKeySet[Int]()
    /** The films whose overview the synopsis evidence asked and the capture holds none of: a fill answers them. */
    val overviews = ConcurrentHashMap.newKeySet[Int]()
    /** The films whose cast the cast evidence asked and the capture holds none of: a fill answers them. */
    val casts = ConcurrentHashMap.newKeySet[Int]()
    def unanswered: Set[String] = queries.asScala.map(DecisionTrace.renderQuery).toSet ++ films.asScala.map(id => s"tmdb $id")
    override def hasDetail(listing: Listing): Boolean = capture.withDetail(ListingKey.serialised(listing.key))
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = known(capture.details.get(ListingKey.serialised(listing.key)))(())
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = known(capture.queries.get(query))(queries.add(query))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = known(capture.films.get(tmdbId))(films.add(tmdbId))
    // a cast the capture did not record is not known — what the cast evidence reads as nothing to go on, never a gap —
    // noted for a fill to answer
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] =
      Answer.Known(capture.casts.get(tmdbId).fold { casts.add(tmdbId); Option.empty[Seq[String]] }(identity))
    // nor is an overview: not known, the evidence reads nothing — noted for a fill to answer
    override def overview(tmdbId: Int): Answer[Option[ProseWords]] =
      Answer.Known(capture.overviews.get(tmdbId).fold { overviews.add(tmdbId); Option.empty[ProseWords] }(identity))
    private def known[A](held: Option[A])(missing: => Any): Answer[A] = held.fold[Answer[A]] { missing; Answer.Unknown }(Answer.Known(_))
  }

  // ── the file ─────────────────────────────────────────────────────────────────────────────

  def write(path: Path, capture: Capture): Unit = {
    val (credits, older) = capture.queries.partition { case (q, _) => q.isInstanceOf[CandidateQuery.TitleCredit] }
    def answered(queries: Map[CandidateQuery, Seq[Hit]]) =
      array(queries.toSeq.sortBy(_._1.sortKey).map { case (q, hits) => queryDoc(q).append("hits", array(hits.map(hitDoc))) })
    val d = new BsonDocument("country", new BsonString(capture.country.code))
      .append("listings", array(capture.listings.sortBy(_.sortKey).map(listingDoc)))
      .append("decisions", array(capture.decisions.sortBy(_.members.headOption.map(ListingKey.serialised).getOrElse("")).map(ResolverDecisionBson.encode)))
      .append("queries", answered(older))
      // apart from "queries", which a ref predating the title-credit question reads whole and fails on a kind it lacks
      .append("titleCredits", answered(credits))
      .append("films", array(capture.films.toSeq.sortBy(_._1).map { case (id, f) => new BsonDocument("id", new BsonInt32(id)).append("film", IdentityAnswerBson.film(f)) }))
      .append("details", array(capture.details.toSeq.sortBy(_._1).map { case (k, f) => new BsonDocument("key", new BsonString(k)).append("detail", f.fold[BsonValue](new BsonNull)(detailDoc)) }))
      .append("withDetail", array(capture.withDetail.toSeq.sorted.map(new BsonString(_))))
      .append("families", array(capture.families.toSeq.sortBy(_._1).map { case (id, doc) => new BsonDocument("_id", new BsonString(id)).append("doc", doc) }))
      .append("finds", array(capture.finds.toSeq.sortBy(_._1).map { case (imdb, tmdb) => new BsonDocument("imdb", new BsonString(imdb)).append("tmdb", tmdb.fold[BsonValue](new BsonNull)(new BsonInt32(_))) }))
      .append("unreachable", array(capture.unreachable.toSeq.sortBy { case (family, question) => (family.ordinal, question) }.map { case (family, question) =>
        new BsonDocument("family", new BsonString(family.label)).append("question", new BsonString(question)) }))
      .append("casts", array(capture.casts.toSeq.sortBy(_._1).map { case (id, cast) => new BsonDocument("id", new BsonInt32(id)).append("cast", cast.fold[BsonValue](new BsonNull)(strings)) }))
      .append("overviews", array(capture.overviews.toSeq.sortBy(_._1).map { case (id, words) =>
        new BsonDocument("id", new BsonInt32(id)).append("words", words.fold[BsonValue](new BsonNull)(w => array(w.hashes.map(new BsonInt32(_))))) }))
      .append("live", array(capture.live.toSeq.sortBy(_._1).map { case (request, body) => new BsonDocument("request", new BsonString(request)).append("body", new BsonString(body)) }))
    Files.createDirectories(path.getParent)
    val out = new GZIPOutputStream(Files.newOutputStream(path))
    try out.write(d.toJson(JsonWriterSettings.builder().outputMode(JsonMode.RELAXED).build()).getBytes(StandardCharsets.UTF_8)) finally out.close()
  }

  def read(path: Path): Capture = {
    val in = new GZIPInputStream(Files.newInputStream(path))
    val d  = try BsonDocument.parse(new String(in.readAllBytes(), StandardCharsets.UTF_8)) finally in.close()
    val country = Country.all.find(_.code == d.getString("country").getValue).getOrElse(sys.error(s"no country in $path"))
    def docs(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asDocument)
    Capture(country,
      docs("listings").map(listingOf),
      docs("decisions").map(ResolverDecisionBson.decode),
      (docs("queries") ++ (if (d.containsKey("titleCredits")) docs("titleCredits") else Nil))
        .map(q => queryOf(q) -> q.getArray("hits").getValues.asScala.toSeq.map(h => hitOf(h.asDocument))).toMap,
      docs("films").map(f => f.getInt32("id").getValue -> IdentityAnswerBson.filmOf(f.get("film"))).toMap,
      docs("details").map(x => x.getString("key").getValue -> Option(x.get("detail")).filterNot(_.isNull).map(v => detailOf(v.asDocument))).toMap,
      d.getArray("withDetail").getValues.asScala.map(_.asString.getValue).toSet,
      docs("families").map(f => f.getString("_id").getValue -> f.getDocument("doc")).toMap,
      docs("finds").map(f => f.getString("imdb").getValue -> Option(f.get("tmdb")).filterNot(_.isNull).map(_.asNumber.intValue)).toMap,
      (if (d.containsKey("casts")) docs("casts") else Nil).map(f => f.getInt32("id").getValue -> Option(f.get("cast")).filterNot(_.isNull).map(_ => stringsOf(f, "cast"))).toMap,
      (if (d.containsKey("unreachable")) docs("unreachable") else Nil).map { u =>
        val label = u.getString("family").getValue
        VoterFamily.values.find(_.label == label).getOrElse(sys.error(s"no family $label in $path")) -> u.getString("question").getValue
      }.toSet,
      (if (d.containsKey("live")) docs("live") else Nil).map(l => l.getString("request").getValue -> l.getString("body").getValue).toMap,
      (if (d.containsKey("overviews")) docs("overviews") else Nil).map(f => f.getInt32("id").getValue ->
        Option(f.get("words")).filterNot(_.isNull).map(w => ProseWords.ofHashes(w.asArray.getValues.asScala.toSeq.map(_.asNumber.intValue)))).toMap)
  }

  private def array(values: Seq[BsonValue]): BsonArray = new BsonArray(values.asJava)
  private def strings(values: Seq[String]): BsonArray = array(values.map(new BsonString(_)))
  private def stringsOf(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))
  private def opt(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).filterNot(_.isNull).map(_.asString.getValue)
  private def optInt(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filterNot(_.isNull).map(_.asNumber.intValue)
  private def put(d: BsonDocument, name: String, value: Option[String]): BsonDocument = { value.foreach(v => d.append(name, new BsonString(v))); d }
  private def putInt(d: BsonDocument, name: String, value: Option[Int]): BsonDocument = { value.foreach(v => d.append(name, new BsonInt32(v))); d }

  private def listingDoc(l: Listing): BsonDocument = {
    val d = new BsonDocument("cinema", new BsonString(l.cinema.displayName)).append("key", new BsonString(ListingKey.serialised(l.key)))
      .append("rawTitle", new BsonString(l.rawTitle)).append("title", new BsonString(l.title)).append("cleanTitle", new BsonString(l.cleanTitle))
      .append("directors", strings(l.directors)).append("countries", strings(l.countries))
      .append("catalogueIds", array(l.catalogueIds.map(c => new BsonDocument("source", new BsonString(c.source)).append("id", new BsonString(c.id)))))
    putInt(d, "year", l.year); putInt(d, "runtime", l.runtime); put(d, "page", l.page); put(d, "originalTitle", l.originalTitle)
    if (!l.screenings.isEmpty) d.append("screenings", strings(l.screenings.days.map(_.toString)))
    if (!l.names.isEmpty) d.append("names", array(l.names.hashes.map(new BsonInt32(_))))
    if (!l.words.isEmpty) d.append("words", array(l.words.hashes.map(new BsonInt32(_))))
    put(d, "searchTitle", l.searchTitle); put(d, "properTitle", l.properTitle); put(d, "poster", l.poster); put(d, "certificate", l.certificate)
  }
  private def listingOf(d: BsonDocument): Listing = {
    val name   = d.getString("cinema").getValue
    val cinema = Source.byDisplayName.get(name).flatMap(Source.cinemaOf).getOrElse(sys.error(s"no venue named $name"))
    Listing(cinema, ListingKey.parse(d.getString("key").getValue).getOrElse(sys.error(s"bad key in ${d.toJson}")), d.getString("rawTitle").getValue,
      d.getString("title").getValue, d.getString("cleanTitle").getValue, optInt(d, "year"), stringsOf(d, "directors"), optInt(d, "runtime"),
      opt(d, "page"), opt(d, "originalTitle"), stringsOf(d, "countries"),
      d.getArray("catalogueIds").getValues.asScala.toSeq.map(_.asDocument).map(c => CatalogueId(c.getString("source").getValue, c.getString("id").getValue)),
      opt(d, "searchTitle"), opt(d, "poster"), ScreeningDays.of(stringsOf(d, "screenings").map(java.time.LocalDate.parse)),
      VenueNames.ofHashes(Option(d.get("names")).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asNumber.intValue))), properTitle = opt(d, "properTitle"),
      certificate = opt(d, "certificate"),
      words = ProseWords.ofHashes(Option(d.get("words")).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asNumber.intValue))))
  }
  private def queryDoc(q: CandidateQuery): BsonDocument = q match {
    case CandidateQuery.Title(text)       => new BsonDocument("title", new BsonString(text))
    case CandidateQuery.Director(name)    => new BsonDocument("director", new BsonString(name))
    case CandidateQuery.Imdb(title)       => new BsonDocument("imdb", new BsonString(title))
    case CandidateQuery.ImdbTitled(title) => new BsonDocument("imdbTitled", new BsonString(title))
    case CandidateQuery.TitleCredit(title) => new BsonDocument("titleCredit", new BsonString(title))
  }
  private def queryOf(d: BsonDocument): CandidateQuery =
    opt(d, "title").map(CandidateQuery.Title(_)).orElse(opt(d, "director").map(CandidateQuery.Director(_)))
      .orElse(opt(d, "imdb").map(CandidateQuery.Imdb(_))).orElse(opt(d, "imdbTitled").map(CandidateQuery.ImdbTitled(_)))
      .orElse(opt(d, "titleCredit").map(CandidateQuery.TitleCredit(_)))
      .getOrElse(sys.error(s"no query in ${d.toJson}"))
  private def hitDoc(h: Hit): BsonDocument = {
    val d = new BsonDocument("id", new BsonInt32(h.tmdbId)).append("title", new BsonString(h.title)).append("popularity", new BsonDouble(h.popularity))
    put(d, "originalTitle", h.originalTitle); putInt(d, "year", h.year)
  }
  private def hitOf(d: BsonDocument): Hit =
    Hit(d.getInt32("id").getValue, d.getString("title").getValue, opt(d, "originalTitle"), optInt(d, "year"), d.getNumber("popularity").doubleValue)
  private def detailDoc(f: DetailFacts): BsonDocument = {
    val d = new BsonDocument("directors", strings(f.directors)).append("countries", strings(f.countries))
    if (f.cast.nonEmpty) d.append("cast", strings(f.cast))
    putInt(d, "year", f.year); putInt(d, "runtime", f.runtime); put(d, "originalTitle", f.originalTitle); put(d, "synopsis", f.synopsis)
  }
  private def detailOf(d: BsonDocument): DetailFacts =
    DetailFacts(optInt(d, "year"), stringsOf(d, "directors"), optInt(d, "runtime"), opt(d, "originalTitle"), stringsOf(d, "countries"),
      opt(d, "synopsis"), stringsOf(d, "cast"))
}
