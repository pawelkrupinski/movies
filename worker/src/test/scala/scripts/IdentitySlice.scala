package scripts

import models.Country
import org.bson.{BsonArray, BsonDocument, BsonString}
import play.api.libs.json.Json
import services.identity._
import services.identity.agreement.{AgreementStage, InMemoryAgreementVerdicts, VoterFamily}
import services.movies.{ListingKey, TitleNormalizer}
import tools.{CatalogueFill, IdentityJudgements, MemoLookups, RecordedCorpus, SliceHttp, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.{Clock, Instant, ZoneOffset}
import java.util.Locale
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * ONE listing's identity, reproduced from a recording in seconds (docs/design/identity-slice.md;
 * `scripts/identity-slice` compiles this at each git ref asked and runs it):
 *
 *  - EXTRACT: open a "Record scrape fixtures" recording Mongo-free ([[RecordedCorpus]]), resolve its corpus whole once
 *    (cached per recording and ref: `index-<cc>.json.gz`), cut the selected listings' families and their corpus
 *    context out ([[IdentitySlicing.select]]) and decide them — model and agreement — through a fetch that notes every
 *    answer read ([[SliceHttp.Noting]]). The slice — its listings and every HTTP answer — is written to a directory:
 *    hermetic, closed, and holding nothing a re-record would not.
 *  - REPLAY: decide a slice again from its directory alone ([[SliceHttp.Replaying]]): a request it lacks fails LOUDLY
 *    (named, exit 3) unless `--recording` is given to widen it from.
 *  - FAITHFULNESS: replay a sample of a convergence leg's listings, each as its own slice, and compare each final take
 *    with the leg's own (`convergence-diagnostics-<cc>-*`'s `<cc>-takes/<cc>.json.gz`).
 *
 * {{{
 *   IdentitySlice <cc> --recording DIR --cache DIR [--slices DIR] [selectors] [--widen N] [--labels TSV] [--out TSV]
 *   IdentitySlice <cc> --replay SLICE_DIR... [--recording DIR] [--labels TSV] [--out TSV]
 *   IdentitySlice <cc> --recording DIR --cache DIR --faithfulness TAKES.json.gz [--sample N] [--seed S] [--out TSV]
 *   selectors: --venue S --title S (substrings, case and accents aside) | --key 'venue|raw title' | --batch FILE
 * }}}
 */
@nowarn("msg=unused import")
object IdentitySlice {
  // The identity adapters, in either package a ref keeps them (OlderRefsPackage).
  import services.identity.adapters.*


  final case class Selector(venue: Option[String], title: Option[String], exact: Boolean) {
    def label: String = (if (exact) "key" else "match") + s" ${venue.getOrElse("*")} | ${title.getOrElse("*")}"
    def selects(l: Listing): Boolean =
      if (exact) venue.forall(_ == l.venue) && title.forall(_ == l.rawTitle)
      else venue.forall(v => folded(l.venue).contains(folded(v))) && title.forall(t => folded(l.rawTitle).contains(folded(t)))
    /** A name for its slice's directory: readable, and unique by a hash of the whole selector. */
    def directoryName: String = {
      val readable = s"${venue.getOrElse("")}-${title.getOrElse("")}".map(c => if (c.isLetterOrDigit) c.toLower else '-').replaceAll("-+", "-").take(60)
      f"$readable-${label.hashCode & 0x7fffffff}%08x"
    }
  }

  final case class Options(country: String = "", recording: Option[Path] = None, cache: Option[Path] = None, slices: Option[Path] = None,
                           selectors: Seq[Selector] = Nil, replay: Seq[Path] = Nil, labels: Option[Path] = None, out: Option[Path] = None,
                           faithfulness: Option[Path] = None, sample: Int = 50, seed: Long = 1, widen: Int = 0, orders: Int = 0, modes: Int = 0, families: Option[Path] = None, ref: String = "",
                           /** The LOCAL prod mirror's URI: prod's filed family answers, stored verdicts and pins read from it ([[tools.ProdMirror]]). */
                           prodMirror: Option[String] = None,
                           /** The worker build prod runs (the `deployed-worker` tag): whose code wrote the mirror's state. */
                           deployed: Option[String] = None,
                           pending: (Option[String], Option[String]) = (None, None), emitCase: Option[Path] = None,
                           /** How many slices are decided at once (one JVM, a bounded pool; the output keeps the batch's order). */
                           threads: Int = math.max(1, math.min(8, Runtime.getRuntime.availableProcessors / 2)),
                           /** Where each slice's result is kept, shared by every checkout (scripts/identity-slice names it per
                            *  recording, code key and filed answers). */
                           results: Option[Path] = None,
                           /** `--serve SOCKET`: keep the session and decide each batch asked over the socket ([[serve]]). */
                           serve: Option[Path] = None, idleMinutes: Int = 30,
                           /** What the whole-corpus index is kept under (scripts/identity-slice's code key: the code that
                            *  resolves it, by content, so a commit changing no code reads the index another commit made);
                            *  the ref when not given. */
                           indexKey: Option[String] = None)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil => flush(o)
    case "--recording" :: v :: rest    => parse(rest, o.copy(recording = Some(Path.of(v))))
    case "--cache" :: v :: rest        => parse(rest, o.copy(cache = Some(Path.of(v))))
    case "--slices" :: v :: rest       => parse(rest, o.copy(slices = Some(Path.of(v))))
    case "--labels" :: v :: rest       => parse(rest, o.copy(labels = Some(Path.of(v))))
    case "--out" :: v :: rest          => parse(rest, o.copy(out = Some(Path.of(v))))
    case "--ref" :: v :: rest          => parse(rest, o.copy(ref = v))
    case "--index-key" :: v :: rest    => parse(rest, o.copy(indexKey = Some(v)))
    case "--faithfulness" :: v :: rest => parse(rest, o.copy(faithfulness = Some(Path.of(v))))
    case "--sample" :: v :: rest       => parse(rest, o.copy(sample = v.toInt))
    case "--seed" :: v :: rest         => parse(rest, o.copy(seed = v.toLong))
    case "--orders" :: v :: rest       => parse(rest, o.copy(orders = v.toInt))
    case "--modes" :: v :: rest        => parse(rest, o.copy(modes = v.toInt))
    case "--families" :: v :: rest     => parse(rest, o.copy(families = Option.when(v != "none")(Path.of(v))))
    case "--prod-mirror" :: v :: rest  => parse(rest, o.copy(prodMirror = Some(v)))
    case "--deployed" :: v :: rest     => parse(rest, o.copy(deployed = Some(v).filter(_.nonEmpty)))
    case "--widen" :: v :: rest        => parse(rest, o.copy(widen = v.toInt))
    case "--threads" :: v :: rest      => parse(rest, o.copy(threads = math.max(1, v.toInt)))
    case "--results" :: v :: rest      => parse(rest, o.copy(results = Some(Path.of(v))))
    case "--serve" :: v :: rest        => parse(rest, o.copy(serve = Some(Path.of(v))))
    case "--idle-minutes" :: v :: rest => parse(rest, o.copy(idleMinutes = v.toInt))
    case "--emit-case" :: v :: rest    => parse(rest, o.copy(emitCase = Some(Path.of(v))))
    case "--replay" :: v :: rest       => parse(rest, o.copy(replay = o.replay :+ Path.of(v)))
    case "--venue" :: v :: rest        => parse(rest, pend(o, venue = Some(v)))
    case "--title" :: v :: rest        => parse(rest, pend(o, title = Some(v)))
    case "--key" :: v :: rest          => parse(rest, flush(o).copy(selectors = flush(o).selectors :+ keySelector(v)))
    case "--batch" :: v :: rest        => parse(rest, flush(o).copy(selectors = flush(o).selectors ++ batch(Path.of(v))))
    case cc :: rest if !cc.startsWith("--") && o.country.isEmpty => parse(rest, o.copy(country = cc.toLowerCase(Locale.ROOT)))
    case other :: _ => sys.error(s"unknown argument $other")
  }
  // `--venue` and `--title` pair up: a second of either starts the next selector
  private def pend(o: Options, venue: Option[String] = None, title: Option[String] = None): Options = {
    val (v, t) = o.pending
    if ((venue.isDefined && v.isDefined) || (title.isDefined && t.isDefined)) pend(flush(o), venue, title)
    else o.copy(pending = (venue.orElse(v), title.orElse(t)))
  }
  private def flush(o: Options): Options = o.pending match {
    case (None, None) => o
    case (v, t)       => o.copy(selectors = o.selectors :+ Selector(v, t, exact = false), pending = (None, None))
  }
  /** `venue|raw title` — `*` as the venue for every venue billing it. */
  def keySelector(text: String): Selector = text.split("\\|", 2) match {
    case Array(venue, title) => Selector(Option(venue.trim).filter(v => v.nonEmpty && v != "*"), Some(title), exact = true)
    case _                   => sys.error(s"--key wants 'venue|raw title': $text")
  }
  /** One selector a line: `venue<TAB>raw title` (exact; `*` any venue), or `--venue S --title S` (substrings). */
  def batch(file: Path): Seq[Selector] = Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toSeq.map(_.trim)
    .filter(l => l.nonEmpty && !l.startsWith("#")).flatMap { line =>
      if (line.startsWith("--")) parse(words(line)).selectors
      else line.split("\t", -1) match {
        case Array(venue, title, _*) => Seq(Selector(Option(venue).filter(v => v.nonEmpty && v != "*"), Some(title), exact = true))
        case _ => sys.error(s"batch line wants 'venue<TAB>raw title' or '--venue S --title S': $line")
      }
    }
  private def words(line: String): List[String] =
    """'([^']*)'|"([^"]*)"|(\S+)""".r.findAllMatchIn(line).map(m => Option(m.group(1)).orElse(Option(m.group(2))).getOrElse(m.group(3))).toList

  def folded(text: String): String = IdentityExplain.folded(text)

  // ── the recording ────────────────────────────────────────────────────────────────────────

  val StubTmdbKey = "identity-shadow"

  /** A recording's directory: `corpus/cinema-scrapes-<cc>.json.gz` and `enrichment-<cc>/` (identity-fetch-recording.sh's
   *  `pair/test/resources/fixtures`), and when it was recorded — the clock every replay of it runs at. */
  final case class Recording(root: Path, country: Country) {
    def corpus: Path = root.resolve("corpus").resolve(s"cinema-scrapes-${country.code}.json.gz")
    def recordedAt: Instant = {
      val coverage = root.resolve("corpus").resolve(s"prod-coverage-${country.code}.json")
      Try((Json.parse(Files.readString(coverage)) \ "recordedAt").as[String]).map(Instant.parse)
        .getOrElse(Files.getLastModifiedTime(corpus).toInstant)
    }
  }

  /** What every decision of a slice reads: its fetch, the lookups over it and the clock. */
  final class Reads(val country: Country, val fetch: tools.HttpFetch, val gaps: TmdbIdentityLookups.Gaps, val clock: Clock,
                    /** The answers no recording holds — prod's filed family answers, the capture's live TMDB ones — read where the recording has none ([[tools.FiledAnswers]]). */
                    val filed: tools.FiledAnswers,
                    /** Prod's state beside the recording — the stage's stored verdicts, the pins — from the local prod mirror, or
                     *  `ProdState.none` in a hermetic slice: no default, every reader says which. */
                    val prod: tools.ProdState) {
    /** For a ref's OWN `IdentityDrift`, which this file replaces in the ref's tree without it (IdentityDrift is not a tool file:
     *  it replays the ref's incremental model, which an older ref's code could not be reached for by name) and which predates
     *  `prod`: its reads hold no prod state. Nothing in this checkout calls it. */
    def this(country: Country, fetch: tools.HttpFetch, gaps: TmdbIdentityLookups.Gaps, clock: Clock, filed: tools.FiledAnswers) =
      this(country, fetch, gaps, clock, filed, tools.ProdState.none)
    val normalizer: TitleNormalizer = TitleNormalizer.forCountry(country)
    val tmdb = OlderRefs.tmdbClient(fetch, settings.TmdbApiKey(StubTmdbKey), country.language)
    private val venueClock = new models.VenueClock(clock)
    /** The country's venue detail pages, read by its scrapers' own detail enrichers over the same fetch. */
    val enrichers: Seq[services.cinemas.common.DetailEnricher] = {
      val catalog = new services.cinemas.CinemaScraperCatalog(fetch, venueClock, clock, normalizer)
      // filed by City since 89fc69800, by the city's slug before it: this file is dropped into refs from either side
      def filedUnder(key: Any, city: models.City) = key match { case slug: String => slug == city.slug; case other => other == city }
      country.cities.flatMap(c => catalog.byCity.collect { case (key, scrapers) if filedUnder(key, c) => scrapers }.flatten)
        .collect { case de: services.cinemas.common.DetailEnricher => de }.distinct
    }
    private val recorded = new TmdbIdentityLookups(tmdb, new services.enrichment.ImdbClient(fetch), enrichers, gaps)
    /** The recording's answers, and the filed ones where it has none — each filed one read noted. */
    val filedLookups: Option[tools.FiledAnswers.Lookups] = Option.when(!filed.isEmpty)(new tools.FiledAnswers.Lookups(recorded, filed))
    /** Every question the decisions over these reads asked and got no answer to: not the recording's, not filed. */
    val asking = new Asking(filedLookups.getOrElse(recorded))
    // the lookups a newer ref added, which no wrapper here overrides (an overview's words): the recording's, as the
    // capture's fill read them
    val lookups: IdentityLookups = OlderRefs.passing(classOf[IdentityLookups], asking, recorded, OlderRefs.NewerLookups)
    val families: Seq[VoterFamily] = UnmatchedClusters.familiesOf(country)
    val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(fetch)),
      new WikiFamily(new services.enrichment.WikidataClient(fetch), country.language.getLanguage),
      new FilmwebFamily(new services.enrichment.FilmwebClient(fetch),
        services.cinemas.pl.FilmwebProgrammes.resolving(fetch, () => venueClock.todayInPoland).of),
      new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(fetch)),
      new MetacriticFamily(new services.enrichment.MetacriticClient(fetch))).filter(s => families.contains(s.family)).map(s => s.family -> s).toMap
  }

  /** `inner`, every question it leaves unanswered noted as a trace renders it (`imdb "When Pigs Fly"`, `tmdb 149933`).
   *  Overrides only what every ref this file is dropped into has (no `released`, no `prefetchAnswered`). */
  final class Asking(inner: IdentityLookups) extends IdentityLookups {
    val unanswered: java.util.Set[String] = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    private def noted[A](answer: Answer[A], question: => String): Answer[A] = { if (answer == Answer.Unknown) unanswered.add(question); answer }
    override def hasDetail(listing: Listing): Boolean = inner.hasDetail(listing)
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = inner.prefetch(queries, films, details)
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = noted(inner.detail(listing), s"page ${listing.page.getOrElse(listing.key.venue)}")
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = noted(inner.candidates(query), DecisionTrace.renderQuery(query))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId), s"tmdb $tmdbId")
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = noted(inner.cast(tmdbId), s"cast $tmdbId")
  }

  /** Why this code's model decides a captured listing otherwise, from the questions it left unanswered: (a) a question
   *  the capture's code never asked — a capture that keeps its live answers ([[tools.FiledAnswers.live]]) holds every
   *  one its model read, so one it lacks was never asked: recapture; (b) without them, an answer the capture's model read
   *  LIVE that neither the recording nor the capture kept, or (a) — the capture predates its live answers; recapture
   *  (or slice at the capture's code) to tell. No question unanswered: the answers are all there and the CODE decides
   *  otherwise. */
  def modelMovedWhy(unanswered: Seq[String], filed: tools.FiledAnswers): String = {
    val named = unanswered.sorted.take(4).mkString("; ") + (if (unanswered.size > 4) s"; … ${unanswered.size - 4} more" else "")
    if (unanswered.isEmpty) "[code] every question answered — this code's model decides it otherwise on the same answers"
    else if (filed.live.nonEmpty) s"[a: new question] asks what the capture's code never did ($named) — recapture"
    else s"[a or b: capture keeps no live answers] unanswered: $named — the capture's model read these live and kept none, or " +
      "never asked them; recapture (it now keeps them) or slice at the capture's code to tell"
  }

  def listingsOf(recording: Recording, rows: Seq[services.scrapes.ArchivedScrape]): Seq[Listing] = {
    val venues = tools.CountryScrapeCorpus.cinemasOf(recording.country).toSet
    // as prod's model reads them (lean), as identity-measure replays the recording
    OlderRefs.modelCorpus(rows.filter(r => venues(r.cinema)).map(r => r.cinema -> r.films).filter(_._2.nonEmpty), TitleNormalizer.forCountry(recording.country))
  }

  // ── deciding a slice: the model, then the agreement ───────────────────────────────────────

  /** A slice decided: the model's decisions on its core, the final ones after the agreement stage, what the stage left
   *  open, and the requests its fetch met as gaps of the recording. */
  final case class Decided(model: Seq[ResolverDecision], agreed: Resolution, open: Set[(VoterFamily, String)], unreachable: Set[(VoterFamily, String)],
                           /** The prod-filed answers the agreement read: what the slice keeps of them. */
                           filedRead: Option[tools.FiledAnswers] = None,
                           /** A CHANGED projection's take of the same model after scrapes of `venues`, as
                            *  `IdentityProjection.tickChanged` hands it ([[services.identity.IdentitySlicing.changedRead]]). */
                           changed: Set[String] => Resolution = _ => Resolution(Nil, 0, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, Map.empty))

  def decide(core: Seq[Listing], context: Seq[Listing], reads: Reads, label: String): Decided =
    agree(IdentitySlicing.resolve(core, context, reads.lookups, reads.normalizer, OlderRefs.resolveInputs(reads.prod.pins, reads.country.code)), core, reads, label)

  /** The slice's listings handed to the INCREMENTAL model in a shuffled order, a few batches at a time — as scrapes reach
   *  the worker's model — and its decisions on the core: what P1 says equals [[IdentitySlicing.resolve]]'s. */
  def incrementally(core: Seq[Listing], context: Seq[Listing], reads: Reads, seed: Long): Seq[ResolverDecision] = {
    val (calibration, pins, decorations) = OlderRefs.resolveInputs(reads.prod.pins, reads.country.code)
    val model = new IncrementalResolver(reads.lookups, reads.normalizer, calibration, pins, decorations)
    val all   = new scala.util.Random(seed).shuffle(core ++ context)
    all.grouped(math.max(1, all.size / 3 + 1)).foreach(model.batch(_, Nil, services.identity.AnswersChanged.Empty))
    val coreKeys = core.map(_.key).toSet
    model.decisions.filter(_.members.exists(coreKeys))
  }

  def agree(model: Seq[ResolverDecision], core: Seq[Listing], reads: Reads, label: String): Decided = {
    // prod's filed family answers first, as the worker's store holds them; the rest asked of the recording
    val docs     = new tools.FiledAnswers.Noting(reads.filed)
    val store    = new FamilyAnswerStore(docs, reads.clock)
    val posters  = new PosterAnswerStore(store, reads.clock)
    val hashing  = new PosterHashing(new services.sharecards.EgressPosterDownload(reads.fetch), new services.sharecards.VipsPosterShrinker(binary = None),
      id => reads.tmdb.posters(id, also = Seq("en")), reads.country.language.getLanguage)
    val catalogue = new CatalogueFill(store, reads.clock, reads.fetch)
    val findsRead = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]]()
    // the recording's find first; one it does not hold, prod's filed one where there is
    def tmdbOf(imdb: String): Answer[Option[Int]] = reads.gaps.answered(reads.tmdb.findByImdbId(imdb).map(_.id)) match {
      case Answer.Unknown => reads.filed.finds.get(imdb).fold[Answer[Option[Int]]](Answer.Unknown) { found => findsRead.put(imdb, found); Answer.Known(found) }
      case known          => known
    }
    val periodic = new AgreementStage(reads.families.map(f => f -> store.answers(f)).toMap, reads.lookups, reads.normalizer, IdentityCalibration.resolver,
      tmdbOf = tmdbOf, stored = storedOf(reads.prod), clock = reads.clock,
      posters = posters, tmdb = Some(reads.lookups), identities = OlderRefs.call(OlderRefs.projectionWiring, "identities", reads.country.code).get.asInstanceOf[Seq[VoterFamily]],
      catalogue = catalogue.answers, listedOn = OlderRefs.call(OlderRefs.projectionWiring, "listedOn", reads.country.code).get.asInstanceOf[Option[VoterFamily]],
      correctionPostersAtOnce = Int.MaxValue, correctionsPerApply = Int.MaxValue)
    val byKey      = core.map(l => l.key -> l).toMap
    val resolution = UnmatchedClusters.resolutionOf(model)
    // a periodic projection: every listing read, nothing to build on
    val asked = quietly(new tools.AgreementQuestionRounds(store, reads.sources, label).run(() => periodic.apply(resolution, byKey.get, store.version, None))(
      _ => OlderRefs.stageWanted(periodic)._1, pending = _ => { val (_, p, c) = OlderRefs.stageWanted(periodic); p.nonEmpty || c.nonEmpty },
      beside = { _ =>
        val (_, wantedPosters, wantedCatalogue) = OlderRefs.stageWanted(periodic)
        tools.ConcurrentAsks.each(wantedPosters.toSeq, atOnce = 8)(q => posters.file(q, Try(hashing.of(q)).getOrElse(Nil)))
        catalogue.file(wantedCatalogue)
      }))
    // a changed projection: the worker's ONE stage (IdentityProjectionWiring's agreementStage, which the periodic projection
    // applied before it) handed every listing, the re-scraped venues' read again
    val changed = (venues: Set[String]) => {
      val read = services.identity.IdentitySlicing.changedRead(core, venues)
      periodic.apply(resolution, read.listingOf, store.version, read.republished)
    }
    Decided(model, asked.outcome, asked.open, asked.unreachable,
      Some(tools.FiledAnswers.read(reads.filed, docs.read.asScala.toMap, findsRead.asScala.toMap, reads.filedLookups)), changed)
  }

  /** The verdicts the stage starts from: prod's stored ones where the slice reads the prod mirror (a verdict stands only
   *  while its listings, rules and every answer it read do), else none. */
  private def storedOf(prod: tools.ProdState): InMemoryAgreementVerdicts = {
    val stored = new InMemoryAgreementVerdicts
    stored.replace(Set.empty, prod.verdicts)
    stored
  }

  /** The rounds' own progress lines, kept off the report. */
  private def quietly[A](body: => A): A = Console.withOut(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))(body)

  // ── a slice on disk ──────────────────────────────────────────────────────────────────────

  /** A slice: its seeds (the selected listings), core and context listings, and every HTTP answer they read. The
   *  listings are kept as the recording's own scrape rows ([[rows]]), and made listings again by each ref's own code —
   *  a change to how a scrape becomes a listing (the title rules, the normaliser) is a change the slice must show. */
  final case class Slice(country: Country, recording: String, recordedAt: Instant, selector: String, seeds: Set[ListingKey],
                         core: Seq[Listing], context: Seq[Listing], http: Map[SliceHttp.Request, SliceHttp.Outcome], extractedAt: Seq[String],
                         rows: Seq[services.scrapes.ArchivedScrape], filed: tools.FiledAnswers)

  /** The recording's `rows`, each venue's films cut to those billing one of `keys`. */
  def rowsOf(rows: Seq[services.scrapes.ArchivedScrape], keys: Set[ListingKey], normalizer: TitleNormalizer): Seq[services.scrapes.ArchivedScrape] =
    rows.flatMap { row =>
      val kept = row.films.filter(film => keys(Listing.of(row.cinema, film, normalizer).key))
      Option.when(kept.nonEmpty)(row.copy(lastSuccess = row.lastSuccess.map(_.copy(films = kept)), lastBarren = None))
    }

  /** Written whole beside `dir`, then moved over it: a slice the cache is shared by every worktree, and another agent
   *  reading it mid-write would read half of one. */
  def writeSlice(dir: Path, slice: Slice): Unit = {
    val part = dir.resolveSibling(s"${dir.getFileName}.part-${ProcessHandle.current.pid}")
    writeSliceFiles(part, slice)
    deleteTree(dir)
    Files.move(part, dir, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
  }

  private def writeSliceFiles(dir: Path, slice: Slice): Unit = {
    Files.createDirectories(dir)
    def keys(ls: Iterable[ListingKey]) = new BsonArray(ls.toSeq.map(ListingKey.serialised).sorted.map(new BsonString(_)).asJava)
    val d = new BsonDocument("country", new BsonString(slice.country.code)).append("recording", new BsonString(slice.recording))
      .append("recordedAt", new BsonString(slice.recordedAt.toString)).append("selector", new BsonString(slice.selector))
      .append("extractedAt", new BsonArray(slice.extractedAt.map(new BsonString(_)).asJava))
      .append("seeds", keys(slice.seeds)).append("core", keys(slice.core.map(_.key))).append("context", keys(slice.context.map(_.key)))
    gzip(dir.resolve("slice.json.gz"), d.toJson)
    gzip(dir.resolve("rows.json.gz"), tools.CorpusFixture.render(slice.rows))
    SliceHttp.write(dir.resolve("http.jsonl.gz"), slice.http)
    tools.FiledAnswers.write(dir, slice.filed)
  }

  def readSlice(dir: Path): Slice = {
    val d = BsonDocument.parse(ungzip(dir.resolve("slice.json.gz")))
    def keys(name: String) = d.getArray(name).getValues.asScala.map(v => ListingKey.parse(v.asString.getValue).get).toSet
    val country  = Country.all.find(_.code == d.getString("country").getValue).get
    val recordedAt = Instant.parse(d.getString("recordedAt").getValue)
    val rows     = tools.CorpusFixture.parse(ungzip(dir.resolve("rows.json.gz")))
    val listings = listingsOf(Recording(dir, country), rows)
    val context  = keys("context")
    Slice(country, d.getString("recording").getValue, recordedAt, d.getString("selector").getValue,
      keys("seeds"), listings.filterNot(l => context(l.key)), listings.filter(l => context(l.key)), SliceHttp.read(dir.resolve("http.jsonl.gz")),
      d.getArray("extractedAt").getValues.asScala.toSeq.map(_.asString.getValue), rows, tools.FiledAnswers.readFrom(dir, country))
  }

  private def gzip(path: Path, text: String): Unit = {
    val out = new java.util.zip.GZIPOutputStream(Files.newOutputStream(path))
    try out.write(text.getBytes(StandardCharsets.UTF_8)) finally out.close()
  }
  private def ungzip(path: Path): String = {
    val in = new java.util.zip.GZIPInputStream(Files.newInputStream(path))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  }

  // ── the whole-corpus index, cached per recording and ref ──────────────────────────────────

  def writeIndex(path: Path, index: IdentitySlicing.Index): Unit = {
    Files.createDirectories(path.getParent)
    val lines = index.listings.map { l =>
      val node = index.nodeOf.getOrElse(l.key, "")
      Json.stringify(Json.obj("k" -> ListingKey.serialised(l.key), "f" -> index.familyOf.getOrElse(l.key, -1), "n" -> node))
    } ++ index.keysOf.toSeq.sortBy(_._1).map { case (node, keys) => Json.stringify(Json.obj("node" -> node, "keys" -> keys.toSeq.sorted)) }
    gzip(path, lines.mkString("", "\n", "\n"))
  }

  def readIndex(path: Path, listings: Seq[Listing]): IdentitySlicing.Index = {
    val lines = ungzip(path).linesIterator.filter(_.nonEmpty).map(Json.parse).toSeq
    val (keyed, nodes) = lines.partition(j => (j \ "k").isDefined)
    val byText = listings.map(l => ListingKey.serialised(l.key) -> l.key).toMap
    val rows = keyed.flatMap(j => byText.get((j \ "k").as[String]).map(k => (k, (j \ "f").as[Int], (j \ "n").as[String])))
    IdentitySlicing.Index(listings, rows.map(r => r._1 -> r._2).toMap, rows.map(r => r._1 -> r._3).toMap,
      nodes.map(j => (j \ "node").as[String] -> (j \ "keys").as[Seq[String]].toSet).toMap, rows.groupMap(_._3)(_._1))
  }

  // ── reporting ────────────────────────────────────────────────────────────────────────────

  /** One listing's final take, as compared between refs and with a convergence leg: the film (`tmdb:N`, the fallback
   *  `source:id`, or `none`), the basis, and the model's film before the agreement. */
  final case class TakeRow(selector: String, key: ListingKey, film: String, title: String, basis: String, confidence: Double, model: String,
                           rules: Seq[String], blocker: String, runnerUp: String, explanation: String, verdict: String) {
    def tsv: String = Seq(selector, ListingKey.serialised(key).replace('\u0000', '|'), key.venue, key.rawTitle, film, title, basis,
      f"$confidence%.3f", model, rules.mkString(" "), blocker, runnerUp, verdict, explanation).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
  }
  val TakeHeader = "selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation"

  def filmOf(d: ResolverDecision): String = d.film.map(id => s"tmdb:$id").orElse(d.fallback.map(f => s"${f.source}:${f.id}")).getOrElse("none")

  def rows(selector: String, wanted: Seq[ListingKey], decided: Decided, labels: Seq[UnmatchedClusters.Label], country: Country,
           films: Int => Option[IdentityMeasures.Film]): Seq[TakeRow] = {
    val model = UnmatchedClusters.resolutionOf(decided.model)
    wanted.flatMap { key =>
      decided.agreed.decisionOf.get(key).map { d =>
        val node   = d.trace.nodes.get(key).orElse(model.decisionOf.get(key).flatMap(_.trace.nodes.get(key)))
        val record = d.film.flatMap(id => decided.agreed.films.get(id).orElse(films(id)))
        val title  = record.map(f => s"${f.title} (${f.year.getOrElse("?")})").orElse(d.fallback.flatMap(f => f.title.map(t => s"$t (${f.year.getOrElse("?")})"))).getOrElse("")
        val take   = UnmatchedClusters.Take(country.code, key.venue, key.rawTitle, d.film, record.filter(_.imdbNumber > 0).map(f => f"tt${f.imdbNumber}%07d")
          .orElse(d.fallback.filter(_.source == "imdb").map(_.id)), d.basis.toString, title, d.agreed, d.fallback.filter(_.source != "imdb").map(f => s"${f.source}:${f.id}"))
        val verdict = if (d.film.isEmpty && d.fallback.isEmpty) "-" else UnmatchedClusters.verdict(take, labels) match {
          case Some(true) => "RIGHT"; case Some(false) => "WRONG"; case None => "UNJUDGED" }
        TakeRow(selector, key, filmOf(d), title, d.basis.toString, d.confidence, model.decisionOf.get(key).map(filmOf).getOrElse("none"),
          d.trace.rulesOf(key) ++ model.decisionOf.get(key).toSeq.flatMap(_.trace.rulesOf(key)).distinct, node.flatMap(_.blocker).getOrElse(""),
          // the stage that decided last appends its why; the model's first line is its own
          node.flatMap(_.candidates.headOption).getOrElse(""), (d.explanation.headOption ++ d.explanation.filterNot(_.startsWith("node ")).lastOption).toSeq.distinct.mkString(" … "), verdict)
      }
    }
  }

  /** The human trace of a slice's wanted listings: billing, the model's decision and its trace, the agreement's, the take
   *  and its verdict with the labels about it, and every record a label calls RIGHT that the candidate pool lacks. */
  def report(slice: Slice, wanted: Seq[ListingKey], decided: Decided, takes: Seq[TakeRow], labels: Seq[UnmatchedClusters.Label]): Unit = {
    val byKey = (slice.core ++ slice.context).map(l => l.key -> l).toMap
    val model = UnmatchedClusters.resolutionOf(decided.model)
    wanted.groupBy(k => decided.agreed.decisionOf.get(k).map(_.members.toSet)).toSeq.sortBy(_._2.map(ListingKey.serialised).min).foreach { case (_, keys) =>
      val first = keys.minBy(ListingKey.serialised)
      println(s"\n== «${first.rawTitle}» — ${keys.map(_.venue).distinct.sorted.mkString(", ")}")
      keys.flatMap(byKey.get).groupBy(l => facts(l)).toSeq.sortBy(_._2.map(_.sortKey).min).foreach { case (line, alike) =>
        println(s"listing ${alike.minBy(_.sortKey).venue} | ${alike.minBy(_.sortKey).rawTitle}${if (alike.size > 1) s" (and ${alike.size - 1} more billed alike)" else ""}")
        println(line)
      }
      model.decisionOf.get(first).foreach { d =>
        println(s"model: ${d.render}")
        d.trace.nodes.get(first).foreach { node =>
          node.searched.foreach(s => println(s"  searched: $s"))
          if (node.candidates.nonEmpty) println(s"  pool: ${node.candidates.take(8).mkString("; ")}")
          node.refusals.foreach(r => println(s"  refused by ${r.rule}: ${r.why}${r.film.fold("")(f => s" (tmdb $f)")}${if (r.detail.nonEmpty) s" — ${r.detail}" else ""}"))
          node.blocker.foreach(b => println(s"  blocker: $b"))
          val pool = node.candidates.map(_.takeWhile(_.isDigit)).toSet
          labels.filter(l => l.country == slice.country.code && IdentityJudgements.normalise(l.rawTitle) == IdentityJudgements.normalise(first.rawTitle) && (l.venue == "*" || l.venue == first.venue) && l.right &&
            l.film.startsWith("tmdb:") && !pool(l.film.drop(5))).foreach(l => println(s"  NOT IN POOL: ${l.film}, which the judgements call right (${l.venue}: ${l.note})"))
        }
      }
      decided.agreed.decisionOf.get(first).foreach { d =>
        println(s"agreement: ${filmOf(d)} (${ResolverDecision.percent(d.confidence)}, ${d.basis})")
        d.explanation.foreach(line => println(s"    $line"))
      }
      takes.filter(t => keys.contains(t.key)).groupBy(t => (t.film, t.title, t.basis, t.verdict)).toSeq.sortBy(_._1._1).foreach { case ((film, title, basis, verdict), at) =>
        val about = labels.filter(l => l.country == slice.country.code && at.exists(t => IdentityJudgements.normalise(t.key.rawTitle) == IdentityJudgements.normalise(l.rawTitle) && (l.venue == "*" || l.venue == t.key.venue)))
          .map(l => s"${if (l.right) "right" else "wrong"} ${l.film} at ${l.venue} (${l.note})").distinct
        println(s"TAKE ${if (at.size == 1) at.head.key.venue else s"at ${at.size} venues"}: $film '$title' via $basis — $verdict" +
          (if (about.nonEmpty) about.mkString(" [", "; ", "]") else ""))
      }
    }
    if (decided.open.nonEmpty) println(s"\nOPEN family questions (the recording holds no answer): ${decided.open.toSeq.map { case (f, q) => s"${f.label} '$q'" }.sorted.mkString(", ")}")
  }

  /** What a listing bills and its stated facts — per listing, never merged by raw title. */
  def facts(l: Listing): String = {
    def names(people: Seq[String]) = if (people.isEmpty) "-" else people.mkString(", ")
    s"  billed: title '${l.title}'${l.originalTitle.fold("")(o => s", original '$o'")}${l.searchTitle.fold("")(s => s", search '$s'")}${OlderRefs.optionalText(l, "properTitle").fold("")(s => s", proper '$s'")}; year ${l.year.getOrElse("-")}, " +
      s"runtime ${l.runtime.getOrElse("-")}, directors ${names(l.directors)}${l.page.fold("")(p => s", page $p")}" +
      s"${if (l.catalogueIds.nonEmpty) s", ids ${l.catalogueIds.map(c => s"${c.source}:${c.id}").mkString(",")}" else ""}" +
      s"${if (!l.screenings.isEmpty) s", screens ${l.screenings.days.headOption.getOrElse("")}…${l.screenings.days.lastOption.getOrElse("")}" else ""}"
  }

  // ── main ─────────────────────────────────────────────────────────────────────────────────

  def main(args: Array[String]): Unit = {
    val o = parse(args.toList)
    val country = Country.all.find(_.code == o.country).getOrElse(sys.error(s"no country '${o.country}'"))
    val labels  = o.labels.filter(Files.exists(_)).fold(Seq.empty[UnmatchedClusters.Label])(IdentityJudgements.labels)
    val started = tools.Stopwatch.start()
    val out     = o.out.map { p => Files.createDirectories(p.toAbsolutePath.getParent); new java.io.PrintWriter(Files.newBufferedWriter(p, StandardCharsets.UTF_8)) }
    out.foreach(_.println(TakeHeader))
    val lacked = scala.collection.mutable.ListBuffer.empty[String]
    // prod's filed family answers, from an unmatched-cluster capture: what no recording holds
    val captured = o.families.filter(Files.exists(_)).fold(tools.FiledAnswers.of(country))(tools.FiledAnswers.ofCapture)
    // and prod's own state, from the local prod mirror, in preference to the capture's — never prod itself
    val mirror = o.prodMirror.map(tools.ProdMirror.open(_, country))
    val (filed, prod) = mirror.fold((captured, tools.ProdState.none)) { m =>
      val started  = tools.Stopwatch.start()
      val families = m.familyAnswers()
      val state    = m.state()
      System.err.println(f"[identity-slice] prod mirror ${m.database}: ${families.size} family answer(s), ${state.verdicts.size} stored verdict(s), " +
        f"${state.pins.size} pin(s) read in ${started.seconds}%.1fs")
      (tools.FiledAnswers.preferring(captured, families, s"prod mirror ${m.database}"), state)
    }
    try {
      if (o.replay.nonEmpty) o.replay.foreach { dir =>
        val slice = readSlice(dir)
        val opened = o.recording.map(r => openRecording(Recording(r, country), filed))
        lacked ++= replaySlice(slice, opened, labels, out, o.orders, o.modes, prod)
      }
      else {
        val recording = Recording(o.recording.getOrElse(sys.error("--recording DIR, or --replay SLICE_DIR")), country)
        val cache     = o.cache.getOrElse(sys.error("--cache DIR (the index of the recording's whole corpus is kept there)"))
        val opened    = openRecording(recording, filed)
        val listings  = listingsOf(recording, opened.rows)
        // the index of a resolve that read a capture's live answers is that capture's own
        val live      = if (filed.live.isEmpty) "" else s"-live${Integer.toHexString(filed.live.keySet.hashCode)}"
        val index     = indexOf(cache.resolve(s"index-${country.code}-${o.indexKey.getOrElse(o.ref)}$live.json.gz"), listings, opened)
        o.faithfulness match {
          case Some(takes) => faithfulness(recording, opened, index, takes, o, filed, out, prod)
          case None =>
            val session = Session(recording, opened, listings, index, filed, cache)
            o.serve match {
              case Some(socket) => serve(socket, session, o)
              case None =>
                if (o.selectors.isEmpty) sys.error("no selector: --venue/--title, --key or --batch")
                lacked ++= slices(session, o, labels, out, prod)
            }
        }
      }
    } finally { out.foreach(_.close()); mirror.foreach(_.close()) }
    System.err.println(f"[identity-slice] ${started.seconds}%.1fs")
    if (lacked.nonEmpty) {
      System.err.println(s"[identity-slice] SLICE INCOMPLETE — the replay asked ${lacked.size} request(s) the slice does not hold (re-run with " +
        s"--recording to widen it from the recording):\n  ${lacked.distinct.take(20).mkString("\n  ")}")
      sys.exit(3)
    }
  }

  // ── many slices: in parallel, each result kept per (recording, ref, slice) ───────────────────

  /** What every slice over one recording at one ref reads: opened once (and kept between batches by `--serve`). */
  final case class Session(recording: Recording, opened: RecordedCorpus.Opened, listings: Seq[Listing], index: IdentitySlicing.Index,
                           filed: tools.FiledAnswers, cache: Path)

  /** One slice decided: its report, its takes' TSV lines, the requests it lacked. */
  final case class SliceResult(report: String, takes: Seq[String], lacked: Seq[String])

  /** The selected slices, decided `o.threads` at a time and printed in the batch's order — the same output whatever the
   *  pool — each result read from, or kept in, `o.results` (a directory per recording, code key and filed answers,
   *  shared by every checkout) under its selector, seeds, options and the labels about its listings: five agents
   *  replaying origin/main over one recording decide each slice once. Returns what the slices lacked. */
  def slices(session: Session, o: Options, labels: Seq[UnmatchedClusters.Label], out: Option[java.io.PrintWriter],
             prod: tools.ProdState): Seq[String] = {
    val country = session.recording.country
    val groups  = sameCluster(o.selectors.map(s => s -> session.listings.filter(s.selects).map(_.key).toSet), session.index, o.widen)
    // a result read from the cache holds nothing of prod's moment, nor the slice an emitted case copies
    val results = o.results.filter(_ => prod.name.isEmpty && o.emitCase.isEmpty)
    def one(selector: Selector, seeds: Set[ListingKey]): SliceResult =
      if (seeds.isEmpty) SliceResult(s"\n## ${selector.label}: no listing of recording ${session.recording.root} matches\n", Nil, Nil)
      else kept(results.map(_.resolve(resultKey(selector, seeds, o, labels, country)))) {
          val report = new java.io.ByteArrayOutputStream()
          val takes  = new java.io.StringWriter()
          val lacked = Console.withOut(new java.io.PrintStream(report, true, StandardCharsets.UTF_8)) {
            inWorkDir(o, session.cache, selector) { dir =>
              val slice = extract(session.recording, session.opened, session.index, seeds, selector.label, o.widen, dir, o.ref, session.filed, prod)
              val lacked = replaySlice(slice, None, labels, Some(new java.io.PrintWriter(takes)), o.orders, o.modes, prod)
              emitInto(slice, dir, labels, o, country, selector)
              lacked
            }
          }
          SliceResult(report.toString(StandardCharsets.UTF_8), takes.toString.linesIterator.filter(_.nonEmpty).toSeq, lacked)
        }
    val threads = math.max(1, math.min(o.threads, groups.size))
    if (groups.size > 1) System.err.println(s"[identity-slice] ${groups.size} slice(s), $threads at a time${results.fold("")(r => s", results kept in $r")}")
    val lacked = scala.collection.mutable.ListBuffer.empty[String]
    inOrder(groups, threads)(one.tupled) { result =>
      print(result.report)
      out.foreach(w => result.takes.foreach(w.println))
      lacked ++= result.lacked
    }
    lacked.toSeq
  }

  /** `f` over `items`, `threads` at a time, each result handed to `each` in the items' order as soon as it and every one
   *  before it are done: the output of a parallel batch is the serial one's. */
  def inOrder[A, B](items: Seq[A], threads: Int)(f: A => B)(each: B => Unit): Unit = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, threads))
    try items.map(item => pool.submit(() => f(item))).foreach(future => each(future.get()))
    finally pool.shutdownNow()
  }

  /** The result kept in `dir`, else `decide`d and kept there (none kept without a `dir`). */
  def kept(dir: Option[Path])(decide: => SliceResult): SliceResult =
    dir.flatMap(readResult).getOrElse {
      val result = decide
      dir.foreach(writeResult(_, result))
      result
    }

  /** `--serve SOCKET`: the session (the recording opened, its listings, the ref's whole-corpus index) kept in this JVM,
   *  and each batch asked of it over a Unix socket decided against it — no JVM start, recording read or index load per
   *  batch. A request is the arguments a batch run takes besides the session's own (selectors, --labels, --out,
   *  --threads, --results, --widen/--orders/--modes), NUL-separated after their length; the answer streams the batch's
   *  output, then `\u0000EXIT <code>`. One request at a time; stops after `--idle-minutes` without one. */
  def serve(socket: Path, session: Session, base: Options): Unit = {
    import java.nio.channels.{Channels, ServerSocketChannel}
    Files.createDirectories(socket.toAbsolutePath.getParent)
    Files.deleteIfExists(socket)
    val server = ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX)
    server.bind(java.net.UnixDomainSocketAddress.of(socket))
    // Since the last request ended; None while one is being served.
    val idleSince = new java.util.concurrent.atomic.AtomicReference(Option(tools.Stopwatch.start()))
    val idle = base.idleMinutes * 60000L
    val watchdog = new Thread(() => while (server.isOpen) {
      Thread.sleep(5000)
      if (idleSince.get.exists(_.millis > idle)) { Files.deleteIfExists(socket); server.close() }
    })
    watchdog.setDaemon(true)
    watchdog.start()
    System.err.println(s"[identity-slice] serving ${session.recording.root} (${session.listings.size} listings) at ${base.ref} on $socket, " +
      s"stopping after ${base.idleMinutes} idle minute(s)")
    try while (true) {
      val client = server.accept()
      idleSince.set(None)
      try {
        val in   = new java.io.DataInputStream(Channels.newInputStream(client))
        val args = new String(in.readNBytes(in.readInt()), StandardCharsets.UTF_8).split('\u0000').toList.filter(_.nonEmpty)
        val answer = new java.io.PrintStream(Channels.newOutputStream(client), true, StandardCharsets.UTF_8)
        val code = Try(Console.withOut(answer)(request(session, base, args))).recover { case e =>
          answer.println(s"[identity-slice] the request failed: $e"); 1 }.get
        answer.print(s"\u0000EXIT $code\n")
        answer.flush()
      } catch { case e: java.io.IOException => System.err.println(s"[identity-slice] a client went away: $e") }
      finally { client.close(); idleSince.set(Some(tools.Stopwatch.start())) }
    } catch { case _: java.nio.channels.ClosedChannelException => System.err.println("[identity-slice] idle: stopped") }
    finally Files.deleteIfExists(socket)
  }

  /** One batch asked of a served session; its exit code (3: some slice lacked a request). */
  private def request(session: Session, base: Options, args: List[String]): Int = {
    val started = tools.Stopwatch.start()
    val o       = parse(args, Options(country = base.country, ref = base.ref, cache = base.cache, slices = base.slices))
    val labels  = o.labels.filter(Files.exists(_)).fold(Seq.empty[UnmatchedClusters.Label])(IdentityJudgements.labels)
    val out     = o.out.map { p => Files.createDirectories(p.toAbsolutePath.getParent); new java.io.PrintWriter(Files.newBufferedWriter(p, StandardCharsets.UTF_8)) }
    out.foreach(_.println(TakeHeader))
    if (o.selectors.isEmpty) sys.error("no selector: --venue/--title, --key or --batch")
    val lacked = try slices(session, o, labels, out, tools.ProdState.none) finally out.foreach(_.close())
    System.err.println(f"[identity-slice] served a request of ${o.selectors.size} selector(s) in ${started.seconds}%.1fs")
    if (lacked.isEmpty) 0
    else {
      println(s"[identity-slice] SLICE INCOMPLETE — ${lacked.size} request(s) the slices do not hold:\n  ${lacked.distinct.take(20).mkString("\n  ")}")
      3
    }
  }

  /** A slice's result's name in the results directory: its selector and seeds, the options that move a take, and the
   *  labels about its listings (a label of another billing changes nothing here; one of these changes the verdict). */
  def resultKey(selector: Selector, seeds: Set[ListingKey], o: Options, labels: Seq[UnmatchedClusters.Label], country: Country): String = {
    val titles = seeds.map(k => IdentityJudgements.normalise(k.rawTitle))
    val about  = labels.filter(l => l.country == country.code && titles(IdentityJudgements.normalise(l.rawTitle)))
      .map(l => Seq(l.venue, l.rawTitle, l.film, l.right, l.note).mkString("\t")).sorted
    val text = (Seq(selector.label, s"widen ${o.widen} orders ${o.orders} modes ${o.modes}") ++ seeds.toSeq.map(ListingKey.serialised).sorted ++ about)
      .mkString("\n")
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8))
    s"${selector.directoryName}-${digest.take(8).map(b => f"$b%02x").mkString}"
  }

  private def readResult(dir: Path): Option[SliceResult] =
    Option.when(Files.isDirectory(dir))(Try(SliceResult(Files.readString(dir.resolve("report.txt")),
      Files.readAllLines(dir.resolve("takes.tsv")).asScala.toSeq, Files.readAllLines(dir.resolve("lacked.txt")).asScala.toSeq)).toOption).flatten

  /** Written whole beside `dir`, then moved over it: another checkout reading it mid-write would read half of one. */
  private def writeResult(dir: Path, result: SliceResult): Unit = Try {
    val part = dir.resolveSibling(s"${dir.getFileName}.part-${ProcessHandle.current.pid}-${Thread.currentThread.threadId}")
    Files.createDirectories(part)
    Files.writeString(part.resolve("report.txt"), result.report)
    Files.write(part.resolve("takes.tsv"), result.takes.asJava)
    Files.write(part.resolve("lacked.txt"), result.lacked.asJava)
    Try(Files.move(part, dir, java.nio.file.StandardCopyOption.ATOMIC_MOVE))
      .recover { case _ => deleteTree(part) }
  }

  /** The recording, a gap it meets answered first by `filed`'s live HTTP answers — what the capture's whole resolve read
   *  live, an IMDb suggestion most often, which its captured model decisions stand on ([[tools.FiledAnswers.LiveLeaf]]). */
  def openRecording(recording: Recording, filed: tools.FiledAnswers): RecordedCorpus.Opened = {
    require(Files.exists(recording.corpus), s"no corpus ${recording.corpus}")
    RecordedCorpus.open(recording.country, recording.corpus, settings.FixtureRoot(recording.root),
      leaf => if (filed.live.isEmpty) leaf else new tools.FiledAnswers.LiveLeaf(filed.live, leaf))
  }

  def readsOf(opened: RecordedCorpus.Opened, fetch: tools.HttpFetch, gaps: TmdbIdentityLookups.Gaps, at: Instant,
              filed: Option[tools.FiledAnswers], prod: tools.ProdState): Reads =
    new Reads(opened.country, fetch, gaps, Clock.fixed(at, ZoneOffset.UTC), filed.getOrElse(tools.FiledAnswers.of(opened.country)), prod)

  /** The recording's whole-corpus index at this ref: read from `path`, else resolved (minutes) and kept there. */
  def indexOf(path: Path, listings: Seq[Listing], opened: RecordedCorpus.Opened): IdentitySlicing.Index =
    if (Files.exists(path)) readIndex(path, listings)
    else {
      val started = tools.Stopwatch.start()
      System.err.println(s"[identity-slice] resolving the recording's ${listings.size} listings whole, once (kept in $path)")
      val reads = readsOf(opened, opened.fetch, opened.gaps, Instant.EPOCH, None, tools.ProdState.none)
      val index = IdentitySlicing.index(listings, OlderRefs.passing(classOf[IdentityLookups], new MemoLookups(reads.lookups), reads.lookups, OlderRefs.NewerLookups), reads.normalizer)
      writeIndex(path, index)
      System.err.println(f"[identity-slice] indexed in ${started.seconds}%.1fs")
      index
    }

  /** Cuts the seeds' slice out of the recording and decides it through a noting fetch; what it read is the slice — merged
   *  into one `dir` holds already (another ref's), so one slice serves every ref asked of it. */
  /** The selectors whose seeds cut the SAME core out of the index, merged into one — the first's, every seed of them — so
   *  one slice decides them all: a batch of 210 CBeebies venues, each "core 219", is one slice, not 210. In order; a
   *  selector matching nothing kept as it is. */
  def sameCluster(selected: Seq[(Selector, Set[ListingKey])], index: IdentitySlicing.Index, widen: Int): Seq[(Selector, Set[ListingKey])] = {
    val groups = scala.collection.mutable.LinkedHashMap.empty[Any, (Selector, Set[ListingKey])]
    selected.zipWithIndex.foreach { case ((selector, seeds), i) =>
      val core: Any = if (seeds.isEmpty) i else IdentitySlicing.select(index, seeds, widen).core
      groups.updateWith(core) { case Some((first, before)) => Some(first -> (before ++ seeds)); case None => Some(selector -> seeds) }
    }
    groups.values.toSeq
  }

  def extract(recording: Recording, opened: RecordedCorpus.Opened, index: IdentitySlicing.Index, seeds: Set[ListingKey], selector: String,
              widen: Int, dir: Path, ref: String, filed: tools.FiledAnswers, prod: tools.ProdState = tools.ProdState.none): Slice = {
    val selection = IdentitySlicing.select(index, seeds, widen)
    // a slice of an older format, or one another agent is replacing, is no slice to merge
    val before    = Option.when(Files.exists(dir.resolve("rows.json.gz")))(Try(readSlice(dir)).toOption).flatten
    val core      = (selection.core ++ before.toSeq.flatMap(_.core.map(_.key))).flatMap(index.byKey.get).toSeq.sortBy(_.sortKey)
    val coreKeys  = core.map(_.key).toSet
    val context   = ((selection.context ++ before.toSeq.flatMap(_.context.map(_.key))) -- coreKeys).flatMap(index.byKey.get).toSeq.sortBy(_.sortKey)
    val noting    = new SliceHttp.Noting(opened.fetch, () => opened.leaf.metOnThread)
    val decided   = decide(core, context, readsOf(opened, noting, opened.gaps, recording.recordedAt, Some(filed), prod), selector)
    val was       = before.fold(tools.FiledAnswers.of(recording.country))(_.filed)
    val slice = Slice(recording.country, recording.root.toString, recording.recordedAt, selector, seeds ++ before.toSeq.flatMap(_.seeds),
      core, context, before.fold(Map.empty[SliceHttp.Request, SliceHttp.Outcome])(_.http) ++ noting.noted.asScala, (before.toSeq.flatMap(_.extractedAt) :+ ref).distinct,
      rowsOf(opened.rows, coreKeys ++ context.map(_.key), TitleNormalizer.forCountry(recording.country)),
      decided.filedRead.fold(was)(tools.FiledAnswers.merge(was, _)))
    writeSlice(dir, slice)
    System.err.println(s"[identity-slice] ${selector}: ${seeds.size} seed(s), core ${core.size}, context ${context.size}, ${slice.http.size} answers → $dir")
    slice
  }

  /** Decides `slice` from its own answers alone (widened from `recording` where given); prints and writes its takes, and
   *  returns what it lacked. */
  def replaySlice(slice: Slice, recording: Option[RecordedCorpus.Opened], labels: Seq[UnmatchedClusters.Label], out: Option[java.io.PrintWriter],
                  orders: Int = 0, modes: Int = 0, prod: tools.ProdState = tools.ProdState.none): Seq[String] = {
    val widen     = recording.map(r => new SliceHttp.Noting(r.fetch, () => r.leaf.metOnThread))
    val replaying = new SliceHttp.Replaying(slice.http, widen)
    val reads     = new Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed, prod)
    val started   = tools.Stopwatch.start()
    val decided   = decide(slice.core, slice.context, reads, slice.selector)
    val wanted    = slice.seeds.toSeq.sortBy(ListingKey.serialised)
    val films     = (id: Int) => reads.lookups.film(id).toOption.flatten
    val takes     = rows(slice.selector, wanted, decided, labels, slice.country, films)
    println(s"\n## ${slice.selector}: ${wanted.size} listing(s); slice core ${slice.core.size}, context ${slice.context.size}, " +
      s"${slice.http.size} answers (${slice.http.values.count(_ == SliceHttp.Outcome.Gap)} recording gaps); decided in " +
      f"${started.seconds}%.1fs at ${slice.recordedAt}")
    report(slice, wanted, decided, takes, labels)
    // what prod's stored state takes for each listing now, and why: beside the slice's take, from the mirror
    if (prod.name.nonEmpty) {
      val sides = prod.sides(wanted)
      println(s"\n-- prod's stored state (local mirror ${prod.name}; the stage started from its ${prod.verdicts.size} stored verdicts, ${prod.pins.size} pin(s)):")
      wanted.foreach(k => sides.get(k).foreach(_.render.foreach(println)))
      val differ = takes.filter(t => sides.get(t.key).exists(_.take._1 != t.film))
      if (differ.nonEmpty) println(s"PROD TAKES OTHERWISE for ${differ.size} of ${takes.size}: " +
        differ.map(t => s"${t.key.venue} | ${t.key.rawTitle}: slice ${t.film}, prod ${sides(t.key).take._1}").take(8).mkString("; "))
    }
    if (!replaying.gapsMet.isEmpty)
      println(s"UNANSWERED by the recording (read as unknown, as the leg reads them): ${replaying.gapsMet.asScala.map(_.render).toSeq.sorted.take(12).mkString("; ")}" +
        (if (replaying.gapsMet.size > 12) s" … ${replaying.gapsMet.size - 12} more" else ""))
    if (replaying.widened.nonEmpty) println(s"WIDENED from the recording: ${replaying.widened.size} answer(s) this ref asked that the slice lacked")
    decided.filedRead.filterNot(_.isEmpty).foreach { read =>
      println(s"FILED, NOT THE RECORDING'S: the decision read ${read.render} the recording does not hold (${slice.filed.source}) — prod's and the capture's live answers")
    }
    if (slice.filed.isEmpty && decided.open.nonEmpty)
      println("NO FILED FAMILY ANSWERS: the agreement read only the recording, which holds none of prod's — its families agree on nothing " +
        "(pass --families <an unmatched-cluster capture> to read prod's)")
    // the projection modes: a periodic projection, then CHANGED ones on the same stage after scrapes of a random half of
    // the slice's venues — every listing still read, the re-scraped venues' read again
    if (modes > 0) {
      val periodic = takes.map(t => t.key -> t.film).toMap
      val venues   = slice.core.map(_.venue).distinct.sorted
      val moved = (1 to modes).flatMap { seed =>
        val subset  = new scala.util.Random(seed).shuffle(venues).take(math.max(1, venues.size / 2)).toSet
        val changed = decided.changed(subset)
        wanted.flatMap(k => changed.decisionOf.get(k).map(filmOf).filter(f => !periodic.get(k).contains(f)).map(f => (seed, k, f, subset(k.venue))))
      }
      if (moved.isEmpty) println(s"PROJECTION MODES: $modes changed projections take what the periodic one takes")
      else moved.groupBy(m => (m._2, m._3)).toSeq.sortBy(_._1._1.venue).foreach { case ((k, film), at) =>
        println(s"PROJECTION MODE FLIP ${k.venue} | ${k.rawTitle}: ${periodic.getOrElse(k, "none")} periodic, $film changed in ${at.size} of $modes " +
          s"(its venue ${if (at.forall(_._4)) "re-scraped" else if (at.forall(!_._4)) "not re-scraped" else "re-scraped or not"})")
      }
    }
    out.foreach(w => takes.foreach(t => w.println(t.tsv)))
    // arrival order: the incremental model fed the slice shuffled, model and agreement, against the whole resolve's takes
    if (orders > 0) {
      val whole = takes.map(t => t.key -> t.film).toMap
      val moved = (1 to orders).flatMap { seed =>
        val ordered = agree(incrementally(slice.core, slice.context, reads, seed), slice.core, reads, s"${slice.selector} order $seed")
        wanted.flatMap(k => ordered.agreed.decisionOf.get(k).map(filmOf).filter(f => !whole.get(k).contains(f)).map(f => (seed, k, f)))
      }
      if (moved.isEmpty) println(s"ARRIVAL ORDER: $orders shuffled orders through the incremental model take what the whole resolve takes")
      else moved.groupBy(m => (m._2, m._3)).foreach { case ((k, film), at) =>
        println(s"ARRIVAL ORDER MOVES ${k.venue} | ${k.rawTitle}: ${whole.getOrElse(k, "none")} whole, $film in ${at.size} of $orders order(s) (seeds ${at.map(_._1).mkString(",")})")
      }
    }
    replaying.lacking.asScala.map(_.render).toSeq.sorted
  }

  // ── a slice pinned as a test case ────────────────────────────────────────────────────────

  /** What a pinned slice's listing must take, as the identity judgements judged it: `film` right for it, with the label's own note.
   *  Set by hand in a case's expected.tsv, `film` may instead be `none` (it takes no film) or `not:<film>` (it takes any
   *  but that one): a listing whose right film its own evidence cannot reach yet must at least not take the wrong one;
   *  either prefixed `pending:` while the fix it waits for has not landed. */
  final case class Expected(venue: String, rawTitle: String, film: String, note: String) {
    /** A `pending:` expectation names a fix not landed yet: the case is pending until it holds. */
    def pending: Boolean = film.startsWith(Expected.Pending)
    def holds(take: Option[String]): Boolean = {
      val wanted = film.stripPrefix(Expected.Pending)
      if (wanted.startsWith(Expected.Not)) !take.contains(wanted.stripPrefix(Expected.Not)) else take.contains(wanted)
    }
  }
  object Expected { val Not = "not:"; val Pending = "pending:" }

  val ExpectedHeader = "venue\trawTitle\tfilm\tlabel"

  def readExpected(dir: Path): Seq[Expected] = Files.readAllLines(dir.resolve("expected.tsv"), StandardCharsets.UTF_8).asScala.toSeq.drop(1)
    .filter(_.nonEmpty).map(_.split("\t", -1)).map(a => Expected(a(0), a(1), a(2), a.lift(3).getOrElse("")))

  /** The right film the identity judgements name for `key` — its venue's own label first, then every venue's (`*`). */
  def rightLabel(key: ListingKey, country: Country, labels: Seq[UnmatchedClusters.Label]): Option[UnmatchedClusters.Label] = {
    val about = labels.filter(l => l.country == country.code && IdentityJudgements.normalise(l.rawTitle) == IdentityJudgements.normalise(key.rawTitle) && l.right)
    about.find(_.venue == key.venue).orElse(about.find(_.venue == "*"))
  }

  /** The films the identity judgements call WRONG for `key` — each by its venue's own label first, then every venue's (`*`):
   *  a film the venue's row calls right is not among them. */
  def wrongLabels(key: ListingKey, country: Country, labels: Seq[UnmatchedClusters.Label]): Seq[UnmatchedClusters.Label] = {
    val about = labels.filter(l => l.country == country.code && IdentityJudgements.normalise(l.rawTitle) == IdentityJudgements.normalise(key.rawTitle) &&
      (l.venue == key.venue || l.venue == "*"))
    about.groupBy(_.film).values.toSeq.flatMap(of => of.find(_.venue == key.venue).orElse(of.find(_.venue == "*"))).filterNot(_.right).sortBy(_.film)
  }

  /** What a seed must take, as the judgements say: the film they call right, or — when they call none right — none of the
   *  films they call wrong (`not:<film>`, one row each): a wrong take judged before its right film is known is still a case. */
  def expectationsOf(key: ListingKey, country: Country, labels: Seq[UnmatchedClusters.Label]): Seq[Expected] =
    rightLabel(key, country, labels).map(label => Seq(Expected(key.venue, key.rawTitle, label.film, s"${label.venue}: ${label.note}")))
      .getOrElse(wrongLabels(key, country, labels).map(label => Expected(key.venue, key.rawTitle, Expected.Not + label.film, s"${label.venue}: ${label.note}")))

  /** Where `selector`'s slice is cut, handed to `f`. The shared work dir (`--slices`, else the cache's: one per recording,
   *  shared by every checkout) keeps a slice between runs, each run merging its listings and answers into the last
   *  ([[extract]]). A case must hold only what THIS run read — this checkout's code and its own filed answers, never
   *  what another checkout (its uncommitted capture, its absolute path) left there — so an emitting run cuts its slice
   *  in a directory of its own, removed once `f` has copied the case out of it. */
  def inWorkDir[A](o: Options, cache: Path, selector: Selector)(f: Path => A): A =
    if (o.emitCase.isEmpty) f(o.slices.getOrElse(cache.resolve("slices")).resolve(selector.directoryName))
    else {
      val own = Files.createTempDirectory("identity-slice-emit")
      try f(own.resolve(selector.directoryName)) finally deleteTree(own)
    }

  /** Under `--emit-case ROOT`, pins `slice` (cut in `dir`) as `ROOT/<cc>-<selector>`, its filed answers named from the
   *  checkout `--families` is in and from the working directory ([[checkoutRelative]]). */
  def emitInto(slice: Slice, dir: Path, labels: Seq[UnmatchedClusters.Label], o: Options, country: Country, selector: Selector): Unit =
    o.emitCase.foreach(root => emitCase(slice, dir, labels, root.resolve(s"${country.code}-${selector.directoryName}"),
      o.families.flatMap(checkoutOf).toSeq :+ Paths.get("")))

  private def deleteTree(dir: Path): Unit =
    if (Files.exists(dir)) Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  /** `source` with every one of `checkouts` taken off the paths it names: a case names its filed answers from a checkout,
   *  not from this machine's path to one — under `--ref` the slice runs in the ref's checkout, and the families file it
   *  read (`--families`) is in the checkout the tool was called from. */
  def checkoutRelative(source: String, checkouts: Seq[Path]): String =
    checkouts.map(_.toAbsolutePath.normalize.toString + java.io.File.separator).distinct.sortBy(-_.length).foldLeft(source)(_.replace(_, ""))

  /** The checkout holding `file`: its nearest directory with a `.git` (a worktree's is a file). */
  def checkoutOf(file: Path): Option[Path] =
    Iterator.iterate(file.toAbsolutePath.normalize.getParent)(_.getParent).takeWhile(_ != null).find(d => Files.exists(d.resolve(".git")))

  /** Copies `slice` (kept in `from`) to `target` with what the judgements expect of each seed ([[expectationsOf]]) — NEVER the
   *  take: a seed they do not judge is left out, and a slice none of whose seeds it judges is refused. Each expectation is
   *  shown beside its label's own note, so a wrong label is seen before it is pinned. */
  def emitCase(slice: Slice, from: Path, labels: Seq[UnmatchedClusters.Label], target: Path, checkouts: Seq[Path] = Seq(Paths.get(""))): Unit = {
    val judged = slice.seeds.toSeq.sortBy(ListingKey.serialised).map(key => key -> expectationsOf(key, slice.country, labels))
    val (known, unknown) = judged.partition(_._2.nonEmpty)
    unknown.foreach { case (key, _) => println(s"--emit-case: the judgements do not judge ${key.venue} | ${key.rawTitle} — not pinned (judge it first)") }
    if (known.isEmpty) println(s"--emit-case REFUSED for ${slice.selector}: the judgements judge none of its ${judged.size} listing(s); judge them first")
    else {
      Files.createDirectories(target)
      Seq("slice.json.gz", "rows.json.gz", "http.jsonl.gz").foreach(name => Files.copy(from.resolve(name), target.resolve(name), java.nio.file.StandardCopyOption.REPLACE_EXISTING))
      // the filed answers the slice's decision read beside the recording's: without them the replay's agreement asks its
      // families afresh, questions the slice never recorded — named from the checkout, not this machine's path to it
      if (!slice.filed.isEmpty) tools.FiledAnswers.write(target, slice.filed.copy(source = checkoutRelative(slice.filed.source, checkouts)))
      val expected = known.flatMap(_._2)
      val lines = expected.map(e => Seq(e.venue, e.rawTitle, e.film, e.note).mkString("\t"))
      Files.writeString(target.resolve("expected.tsv"), (ExpectedHeader +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
      println(s"\n--emit-case: ${known.size} listing(s) pinned in $target (IdentitySliceCasesSpec replays it):")
      expected.foreach(e => println(s"  ${e.venue} | ${e.rawTitle} must take ${e.film} — label ${e.note}"))
    }
  }

  /** Each listing of a pinned slice and the film it takes now: what [[readExpected]] is held to. */
  def takesOf(dir: Path): Map[(String, String), String] = {
    val slice     = readSlice(dir)
    val replaying = new SliceHttp.Replaying(slice.http)
    val reads     = new Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC), slice.filed, tools.ProdState.none)
    val decided   = decide(slice.core, slice.context, reads, slice.selector)
    require(replaying.lacking.isEmpty, s"the slice $dir lacks answers: ${replaying.lacking.asScala.map(_.render).toSeq.sorted.take(5).mkString("; ")}")
    slice.seeds.toSeq.flatMap(key => decided.agreed.decisionOf.get(key).map(d => (key.venue, key.rawTitle) -> filmOf(d))).toMap
  }

  // ── faithfulness: a slice's take against the whole convergence leg's ────────────────────────

  /** The bases the agreement stage takes or moves a film by. */
  val AgreementTakes: Set[ResolverDecision.Basis] = { import ResolverDecision.Basis._; Set(Agreed, Poster, Broadcast, Filled, Catalogue, Withdrawn, Corrected) }

  def faithfulness(recording: Recording, opened: RecordedCorpus.Opened, index: IdentitySlicing.Index, takesPath: Path, o: Options,
                   filed: tools.FiledAnswers, out: Option[java.io.PrintWriter], prod: tools.ProdState = tools.ProdState.none): Unit = {
    val leg     = UnmatchedClusters.read(takesPath)
    // a convergence leg's takes as it took them; an unmatched-cluster capture's (it holds prod's filed family answers) as
    // this code's agreement stage takes them over its captured model decisions — the agreement-taken listings no hermetic
    // leg has, and only those
    val ofCapture = leg.families.nonEmpty
    val legDecisions = if (ofCapture) UnmatchedClusters.replay(leg).agreed.decisions else leg.decisions
    val legOf   = legDecisions.flatMap(d => d.members.map(_ -> d)).toMap
    val ours    = index.byKey
    val rnd     = new scala.util.Random(o.seed)
    val inBoth  = leg.listings.map(_.key).filter(ours.contains)
      .filter(k => !ofCapture || legOf.get(k).exists(d => AgreementTakes(d.basis)))
    // every basis the leg decided by — model, agreement and no-match alike — an equal share, the rest topped up at random
    val byKind  = inBoth.groupBy(k => legOf.get(k).map(_.basis.toString).getOrElse("missing")).toSeq.sortBy(_._1)
      .map { case (kind, keys) => kind -> rnd.shuffle(keys.sortBy(ListingKey.serialised)) }
    val share   = math.max(1, o.sample / math.max(1, byKind.size))
    val even    = byKind.flatMap(_._2.take(share))
    val sample  = (even ++ rnd.shuffle(byKind.flatMap(_._2.drop(share))).take(math.max(0, o.sample - even.size))).take(o.sample)
    println(s"# faithfulness: ${sample.size} listing(s) of ${takesPath} (${leg.listings.size} listings; ${inBoth.size} in this recording) by the leg's basis: " +
      sample.groupBy(k => legOf.get(k).map(_.basis.toString).getOrElse("-")).map { case (b, ks) => s"$b ${ks.size}" }.toSeq.sorted.mkString(", "))
    var alike = 0
    var agreeWider = 0
    var agreeStage = 0
    // with the prod mirror: the slice's take against what prod's stored state takes for the listing NOW
    var agreeProd  = 0
    // (c) prod's stored state was written by the DEPLOYED build: a take this code decides otherwise may be the code's move
    val otherBuild = o.deployed.filterNot(d => o.ref.startsWith(d) || d.startsWith(o.ref))
      .map(d => s"prod's state is the deployed build ${d.take(9)}'s, this slice's code is ${if (o.ref.startsWith("worktree")) s"this checkout's (${o.ref})" else o.ref.take(9) + "'s"}")
    var prodKnown  = 0
    val misses = scala.collection.mutable.ListBuffer.empty[String]
    sample.zipWithIndex.foreach { case (key, i) =>
      val started   = tools.Stopwatch.start()
      val selection = IdentitySlicing.select(index, Set(key), o.widen)
      val core      = selection.core.flatMap(ours.get).toSeq.sortBy(_.sortKey)
      val context   = selection.context.flatMap(ours.get).toSeq.sortBy(_.sortKey)
      val reads     = readsOf(opened, opened.fetch, opened.gaps, recording.recordedAt, Some(filed), prod)
      val decided   = decide(core, context, reads, s"faithfulness $i")
      val asked     = reads.asking.unanswered.asScala.toSeq
      val mine      = decided.agreed.decisionOf.get(key).map(filmOf).getOrElse("missing")
      val theirs    = legOf.get(key).map(filmOf).getOrElse("missing")
      val same      = mine == theirs
      val prodTake  = if (prod.name.isEmpty) None else prod.sides(Seq(key)).get(key).map(_.take).filterNot(_._1 == "missing")
      prodTake.foreach { case (film, _) => prodKnown += 1; if (film == mine) agreeProd += 1 }
      val prodLine  = prodTake.fold("")(t => s"\tprod ${t._1} (${t._2}): ${if (t._1 == mine) "SAME" else s"DIFF${otherBuild.fold("")(b => s" [c: $b]")}"}")
      if (same) alike += 1
      val legMembers = legOf.get(key).toSeq.flatMap(_.members).toSet
      val outside    = legMembers -- core.map(_.key)
      // a capture's: the agreement stage alone, over the CAPTURED model decisions of the slice's core — what the capture's
      // replay hands it — and whether this code's model decides the listing as the captured one did
      val (stageLine, modelMoved) = if (!ofCapture) ("", "") else {
        val coreKeys = core.map(_.key).toSet
        val captured = leg.decisions.filter(_.members.exists(coreKeys))
        val staged   = agree(captured, core, readsOf(opened, opened.fetch, opened.gaps, recording.recordedAt, Some(filed), prod), s"faithfulness $i stage")
        val film     = staged.agreed.decisionOf.get(key).map(filmOf).getOrElse("missing")
        if (film == theirs) agreeStage += 1
        val was = leg.decisions.find(_.members.contains(key))
        val now = UnmatchedClusters.resolutionOf(decided.model).decisionOf.get(key)
        (s"\tstage over the captured model: ${if (film == theirs) "SAME" else s"DIFF $film"}",
          if (was.map(d => (filmOf(d), d.unanswered)) == now.map(d => (filmOf(d), d.unanswered))) ""
          else s"this code's model decides it otherwise than the captured one (${now.fold("-")(d => s"${d.basis}, ${d.unanswered} unanswered")} against " +
            s"${was.fold("-")(d => s"${d.basis}, ${d.unanswered} unanswered")}): ${modelMovedWhy(asked, filed)}")
      }
      // a slice that differs is decided again one ring wider: what the next investigation of it should ask for
      def widened = {
        val wider = IdentitySlicing.select(index, Set(key), o.widen + 1)
        val again = decide(wider.core.flatMap(ours.get).toSeq.sortBy(_.sortKey), wider.context.flatMap(ours.get).toSeq.sortBy(_.sortKey),
          readsOf(opened, opened.fetch, opened.gaps, recording.recordedAt, Some(filed), prod), s"faithfulness $i wider")
        val film = again.agreed.decisionOf.get(key).map(filmOf).getOrElse("missing")
        if (film == theirs) agreeWider += 1
        s"; --widen ${o.widen + 1} (core ${wider.core.size}): ${if (film == theirs) "SAME as the leg" else s"still $film"}"
      }
      val why = if (same) "" else if (modelMoved.nonEmpty) modelMoved else (if (outside.nonEmpty) s"the leg's cluster holds ${outside.size} listing(s) outside the slice's core (e.g. ${outside.take(2).map(k => s"${k.venue} | ${k.rawTitle}").mkString("; ")})"
                else "same members — the slice lacks context the whole corpus gave it (a neighbouring family's facts or agreement)") + widened
      val line = f"${if (same) "SAME" else "DIFF"}\t${key.venue}\t${key.rawTitle}\tslice $mine (${decided.agreed.decisionOf.get(key).map(_.basis.toString).getOrElse("-")})\t" +
        f"leg $theirs (${legOf.get(key).map(_.basis.toString).getOrElse("-")})\tcore ${core.size} context ${context.size}\t${started.seconds}%.1fs\t$why$stageLine$prodLine"
      println(line)
      out.foreach(_.println(line))
      if (!same) misses += line
    }
    println(f"\n# faithfulness: $alike of ${sample.size} listings take what the whole leg took (${100.0 * alike / math.max(1, sample.size)}%.1f%%)")
    if (misses.nonEmpty) println(s"# slice ≠ whole corpus for ${misses.size}: one ring wider, $agreeWider of them take what the leg took (the DIFF lines above)")
    if (ofCapture) println(f"# the agreement stage alone, over the captured model decisions: $agreeStage of ${sample.size} take what the capture's replay took " +
      f"(${100.0 * agreeStage / math.max(1, sample.size)}%.1f%%)")
    if (prod.name.nonEmpty) println(f"# against prod's stored state now (local mirror ${prod.name}): $agreeProd of $prodKnown the mirror holds take what prod " +
      f"takes (${100.0 * agreeProd / math.max(1, prodKnown)}%.1f%%); ${sample.size - prodKnown} not in prod's model now" +
      otherBuild.fold("")(b => s"\n#   $b — slice at the deployed build (--ref ${o.deployed.get.take(9)}) to compare like for like"))
  }
}
