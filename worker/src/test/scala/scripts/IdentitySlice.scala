package scripts

import models.Country
import org.bson.{BsonArray, BsonDocument, BsonString}
import play.api.libs.json.Json
import services.identity._
import services.identity.agreement.{AgreementStage, InMemoryAgreementVerdicts, VoterFamily}
import services.movies.{ListingKey, TitleNormalizer}
import tools.{CatalogueFill, MemoLookups, RecordedCorpus, SliceHttp, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Clock, Instant, ZoneOffset}
import java.util.Locale
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
object IdentitySlice {

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
                           faithfulness: Option[Path] = None, sample: Int = 50, seed: Long = 1, widen: Int = 0, orders: Int = 0, ref: String = "",
                           pending: (Option[String], Option[String]) = (None, None), emitCase: Option[Path] = None)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil => flush(o)
    case "--recording" :: v :: rest    => parse(rest, o.copy(recording = Some(Path.of(v))))
    case "--cache" :: v :: rest        => parse(rest, o.copy(cache = Some(Path.of(v))))
    case "--slices" :: v :: rest       => parse(rest, o.copy(slices = Some(Path.of(v))))
    case "--labels" :: v :: rest       => parse(rest, o.copy(labels = Some(Path.of(v))))
    case "--out" :: v :: rest          => parse(rest, o.copy(out = Some(Path.of(v))))
    case "--ref" :: v :: rest          => parse(rest, o.copy(ref = v))
    case "--faithfulness" :: v :: rest => parse(rest, o.copy(faithfulness = Some(Path.of(v))))
    case "--sample" :: v :: rest       => parse(rest, o.copy(sample = v.toInt))
    case "--seed" :: v :: rest         => parse(rest, o.copy(seed = v.toLong))
    case "--orders" :: v :: rest       => parse(rest, o.copy(orders = v.toInt))
    case "--widen" :: v :: rest        => parse(rest, o.copy(widen = v.toInt))
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
  final class Reads(val country: Country, val fetch: tools.HttpFetch, val gaps: TmdbIdentityLookups.Gaps, val clock: Clock) {
    val normalizer: TitleNormalizer = TitleNormalizer.forCountry(country)
    val tmdb = new clients.TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = country.language, retrySleep = (_: Long) => ())
    private val venueClock = new models.VenueClock(clock)
    /** The country's venue detail pages, read by its scrapers' own detail enrichers over the same fetch. */
    val enrichers: Seq[services.cinemas.common.DetailEnricher] = {
      val catalog = new services.cinemas.CinemaScraperCatalog(fetch, venueClock, clock, normalizer)
      country.cities.flatMap(c => catalog.byCity.getOrElse(c, Nil)).collect { case de: services.cinemas.common.DetailEnricher => de }.distinct
    }
    val lookups: IdentityLookups = new TmdbIdentityLookups(tmdb, new services.enrichment.ImdbClient(fetch), enrichers, gaps)
    val families: Seq[VoterFamily] = UnmatchedClusters.familiesOf(country)
    val sources: Map[VoterFamily, FamilySource] = Seq(new ImdbFamily(new services.enrichment.ImdbClient(fetch)),
      new WikiFamily(new services.enrichment.WikidataClient(fetch), country.language.getLanguage),
      new FilmwebFamily(new services.enrichment.FilmwebClient(fetch),
        services.cinemas.pl.FilmwebProgrammes.resolving(fetch, () => venueClock.todayInPoland).of),
      new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(fetch)),
      new MetacriticFamily(new services.enrichment.MetacriticClient(fetch))).filter(s => families.contains(s.family)).map(s => s.family -> s).toMap
  }

  def listingsOf(recording: Recording, rows: Seq[services.scrapes.ArchivedScrape]): Seq[Listing] = {
    val venues = tools.CountryScrapeCorpus.cinemasOf(recording.country).toSet
    Listing.corpus(rows.filter(r => venues(r.cinema)).map(r => r.cinema -> r.films).filter(_._2.nonEmpty), TitleNormalizer.forCountry(recording.country))
  }

  // ── deciding a slice: the model, then the agreement ───────────────────────────────────────

  /** A slice decided: the model's decisions on its core, the final ones after the agreement stage, what the stage left
   *  open, and the requests its fetch met as gaps of the recording. */
  final case class Decided(model: Seq[ResolverDecision], agreed: Resolution, open: Set[(VoterFamily, String)], unreachable: Set[(VoterFamily, String)])

  def decide(core: Seq[Listing], context: Seq[Listing], reads: Reads, label: String): Decided =
    agree(IdentitySlicing.resolve(core, context, reads.lookups, reads.normalizer), core, reads, label)

  /** The slice's listings handed to the INCREMENTAL model in a shuffled order, a few batches at a time — as scrapes reach
   *  the worker's model — and its decisions on the core: what P1 says equals [[IdentitySlicing.resolve]]'s. */
  def incrementally(core: Seq[Listing], context: Seq[Listing], reads: Reads, seed: Long): Seq[ResolverDecision] = {
    val model = new IncrementalResolver(reads.lookups, reads.normalizer, IdentityCalibration.resolver)
    val all   = new scala.util.Random(seed).shuffle(core ++ context)
    all.grouped(math.max(1, all.size / 3 + 1)).foreach(model.listingsSeen)
    val coreKeys = core.map(_.key).toSet
    model.decisions.filter(_.members.exists(coreKeys))
  }

  def agree(model: Seq[ResolverDecision], core: Seq[Listing], reads: Reads, label: String): Decided = {
    val store    = new FamilyAnswerStore(new InMemoryTmdbDocuments, reads.clock)
    val posters  = new PosterAnswerStore(store, reads.clock)
    val hashing  = new PosterHashing(new services.sharecards.EgressPosterDownload(reads.fetch), new services.sharecards.VipsPosterShrinker(binary = None),
      id => reads.tmdb.posters(id, also = Seq("en")), reads.country.language.getLanguage)
    val catalogue = new CatalogueFill(store, reads.clock, reads.fetch)
    val stage = new AgreementStage(reads.families.map(f => f -> store.answers(f)).toMap, reads.lookups, reads.normalizer, IdentityCalibration.resolver,
      tmdbOf = imdb => reads.gaps.answered(reads.tmdb.findByImdbId(imdb).map(_.id)), stored = new InMemoryAgreementVerdicts, clock = reads.clock,
      posters = posters, tmdb = Some(reads.lookups), identities = modules.wiring.IdentityCutoverWiring.identities(reads.country.code),
      catalogue = catalogue.answers, listedOn = modules.wiring.IdentityCutoverWiring.listedOn(reads.country.code),
      correctionPostersAtOnce = Int.MaxValue, correctionsPerApply = Int.MaxValue)
    val byKey = core.map(l => l.key -> l).toMap
    val asked = quietly(new tools.AgreementQuestionRounds(store, reads.sources, label).run(() => stage.apply(UnmatchedClusters.resolutionOf(model), byKey.get, store.version))(
      _ => stage.wanted, pending = _ => stage.wantedPosters.nonEmpty || stage.wantedCatalogue.nonEmpty,
      beside = { _ =>
        tools.ConcurrentAsks.each(stage.wantedPosters.toSeq, atOnce = 8)(q => posters.file(q, Try(hashing.of(q)).getOrElse(Nil)))
        catalogue.file(stage.wantedCatalogue)
      }))
    Decided(model, asked.outcome, asked.open, asked.unreachable)
  }

  /** The rounds' own progress lines, kept off the report. */
  private def quietly[A](body: => A): A = Console.withOut(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))(body)

  // ── a slice on disk ──────────────────────────────────────────────────────────────────────

  /** A slice: its seeds (the selected listings), core and context listings, and every HTTP answer they read. The
   *  listings are kept as the recording's own scrape rows ([[rows]]), and made listings again by each ref's own code —
   *  a change to how a scrape becomes a listing (the title rules, the normaliser) is a change the slice must show. */
  final case class Slice(country: Country, recording: String, recordedAt: Instant, selector: String, seeds: Set[ListingKey],
                         core: Seq[Listing], context: Seq[Listing], http: Map[SliceHttp.Request, SliceHttp.Outcome], extractedAt: Seq[String],
                         rows: Seq[services.scrapes.ArchivedScrape])

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
    if (Files.exists(dir)) Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
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
      d.getArray("extractedAt").getValues.asScala.toSeq.map(_.asString.getValue), rows)
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
          labels.filter(l => l.country == slice.country.code && l.rawTitle == first.rawTitle && (l.venue == "*" || l.venue == first.venue) && l.right &&
            l.film.startsWith("tmdb:") && !pool(l.film.drop(5))).foreach(l => println(s"  NOT IN POOL: ${l.film}, which labels.tsv calls right (${l.venue}: ${l.note})"))
        }
      }
      decided.agreed.decisionOf.get(first).foreach { d =>
        println(s"agreement: ${filmOf(d)} (${ResolverDecision.percent(d.confidence)}, ${d.basis})")
        d.explanation.foreach(line => println(s"    $line"))
      }
      takes.filter(t => keys.contains(t.key)).groupBy(t => (t.film, t.title, t.basis, t.verdict)).toSeq.sortBy(_._1._1).foreach { case ((film, title, basis, verdict), at) =>
        val about = labels.filter(l => l.country == slice.country.code && at.exists(t => t.key.rawTitle == l.rawTitle && (l.venue == "*" || l.venue == t.key.venue)))
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
    s"  billed: title '${l.title}'${l.originalTitle.fold("")(o => s", original '$o'")}${l.searchTitle.fold("")(s => s", search '$s'")}; year ${l.year.getOrElse("-")}, " +
      s"runtime ${l.runtime.getOrElse("-")}, directors ${names(l.directors)}${l.page.fold("")(p => s", page $p")}" +
      s"${if (l.catalogueIds.nonEmpty) s", ids ${l.catalogueIds.map(c => s"${c.source}:${c.id}").mkString(",")}" else ""}" +
      s"${if (!l.screenings.isEmpty) s", screens ${l.screenings.days.headOption.getOrElse("")}…${l.screenings.days.lastOption.getOrElse("")}" else ""}"
  }

  // ── main ─────────────────────────────────────────────────────────────────────────────────

  def main(args: Array[String]): Unit = {
    val o = parse(args.toList)
    val country = Country.all.find(_.code == o.country).getOrElse(sys.error(s"no country '${o.country}'"))
    val labels  = o.labels.filter(Files.exists(_)).fold(Seq.empty[UnmatchedClusters.Label])(UnmatchedClusters.readLabels)
    val started = System.nanoTime()
    val out     = o.out.map { p => Files.createDirectories(p.toAbsolutePath.getParent); new java.io.PrintWriter(Files.newBufferedWriter(p, StandardCharsets.UTF_8)) }
    out.foreach(_.println(TakeHeader))
    val lacked = scala.collection.mutable.ListBuffer.empty[String]
    try {
      if (o.replay.nonEmpty) o.replay.foreach { dir =>
        val slice = readSlice(dir)
        val opened = o.recording.map(r => openRecording(Recording(r, country)))
        lacked ++= replaySlice(slice, opened, labels, out, o.orders)
      }
      else {
        val recording = Recording(o.recording.getOrElse(sys.error("--recording DIR, or --replay SLICE_DIR")), country)
        val cache     = o.cache.getOrElse(sys.error("--cache DIR (the index of the recording's whole corpus is kept there)"))
        val opened    = openRecording(recording)
        val listings  = listingsOf(recording, opened.rows)
        val index     = indexOf(cache.resolve(s"index-${country.code}-${o.ref}.json.gz"), listings, opened)
        o.faithfulness match {
          case Some(takes) => faithfulness(recording, opened, index, takes, o, labels, out)
          case None =>
            if (o.selectors.isEmpty) sys.error("no selector: --venue/--title, --key or --batch")
            o.selectors.foreach { selector =>
              val seeds = listings.filter(selector.selects).map(_.key).toSet
              if (seeds.isEmpty) println(s"\n## ${selector.label}: no listing of recording ${recording.root} matches")
              else {
                val dir   = o.slices.getOrElse(cache.resolve("slices")).resolve(selector.directoryName)
                val slice = extract(recording, opened, index, seeds, selector.label, o.widen, dir, o.ref)
                lacked ++= replaySlice(slice, None, labels, out, o.orders)
                o.emitCase.foreach(target => emitCase(slice, dir, labels, target.resolve(s"${country.code}-${selector.directoryName}")))
              }
            }
        }
      }
    } finally out.foreach(_.close())
    System.err.println(f"[identity-slice] ${(System.nanoTime() - started) / 1e9}%.1fs")
    if (lacked.nonEmpty) {
      System.err.println(s"[identity-slice] SLICE INCOMPLETE — the replay asked ${lacked.size} request(s) the slice does not hold (re-run with " +
        s"--recording to widen it from the recording):\n  ${lacked.distinct.take(20).mkString("\n  ")}")
      sys.exit(3)
    }
  }

  def openRecording(recording: Recording): RecordedCorpus.Opened = {
    require(Files.exists(recording.corpus), s"no corpus ${recording.corpus}")
    RecordedCorpus.open(recording.country, recording.corpus, settings.FixtureRoot(recording.root))
  }

  def readsOf(opened: RecordedCorpus.Opened, fetch: tools.HttpFetch, gaps: TmdbIdentityLookups.Gaps, at: Instant): Reads =
    new Reads(opened.country, fetch, gaps, Clock.fixed(at, ZoneOffset.UTC))

  /** The recording's whole-corpus index at this ref: read from `path`, else resolved (minutes) and kept there. */
  def indexOf(path: Path, listings: Seq[Listing], opened: RecordedCorpus.Opened): IdentitySlicing.Index =
    if (Files.exists(path)) readIndex(path, listings)
    else {
      val started = System.nanoTime()
      System.err.println(s"[identity-slice] resolving the recording's ${listings.size} listings whole, once (kept in $path)")
      val reads = readsOf(opened, opened.fetch, opened.gaps, Instant.EPOCH)
      val index = IdentitySlicing.index(listings, new MemoLookups(reads.lookups), reads.normalizer)
      writeIndex(path, index)
      System.err.println(f"[identity-slice] indexed in ${(System.nanoTime() - started) / 1e9}%.1fs")
      index
    }

  /** Cuts the seeds' slice out of the recording and decides it through a noting fetch; what it read is the slice — merged
   *  into one `dir` holds already (another ref's), so one slice serves every ref asked of it. */
  def extract(recording: Recording, opened: RecordedCorpus.Opened, index: IdentitySlicing.Index, seeds: Set[ListingKey], selector: String,
              widen: Int, dir: Path, ref: String): Slice = {
    val selection = IdentitySlicing.select(index, seeds, widen)
    // a slice of an older format, or one another agent is replacing, is no slice to merge
    val before    = Option.when(Files.exists(dir.resolve("rows.json.gz")))(Try(readSlice(dir)).toOption).flatten
    val core      = (selection.core ++ before.toSeq.flatMap(_.core.map(_.key))).flatMap(index.byKey.get).toSeq.sortBy(_.sortKey)
    val coreKeys  = core.map(_.key).toSet
    val context   = ((selection.context ++ before.toSeq.flatMap(_.context.map(_.key))) -- coreKeys).flatMap(index.byKey.get).toSeq.sortBy(_.sortKey)
    val noting    = new SliceHttp.Noting(opened.fetch, () => opened.leaf.metOnThread)
    decide(core, context, readsOf(opened, noting, opened.gaps, recording.recordedAt), selector)
    val slice = Slice(recording.country, recording.root.toString, recording.recordedAt, selector, seeds ++ before.toSeq.flatMap(_.seeds),
      core, context, before.fold(Map.empty[SliceHttp.Request, SliceHttp.Outcome])(_.http) ++ noting.noted.asScala, (before.toSeq.flatMap(_.extractedAt) :+ ref).distinct,
      rowsOf(opened.rows, coreKeys ++ context.map(_.key), TitleNormalizer.forCountry(recording.country)))
    writeSlice(dir, slice)
    System.err.println(s"[identity-slice] ${selector}: ${seeds.size} seed(s), core ${core.size}, context ${context.size}, ${slice.http.size} answers → $dir")
    slice
  }

  /** Decides `slice` from its own answers alone (widened from `recording` where given); prints and writes its takes, and
   *  returns what it lacked. */
  def replaySlice(slice: Slice, recording: Option[RecordedCorpus.Opened], labels: Seq[UnmatchedClusters.Label], out: Option[java.io.PrintWriter],
                  orders: Int = 0): Seq[String] = {
    val widen     = recording.map(r => new SliceHttp.Noting(r.fetch, () => r.leaf.metOnThread))
    val replaying = new SliceHttp.Replaying(slice.http, widen)
    val reads     = new Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC))
    val started   = System.nanoTime()
    val decided   = decide(slice.core, slice.context, reads, slice.selector)
    val wanted    = slice.seeds.toSeq.sortBy(ListingKey.serialised)
    val films     = (id: Int) => reads.lookups.film(id).toOption.flatten
    val takes     = rows(slice.selector, wanted, decided, labels, slice.country, films)
    println(s"\n## ${slice.selector}: ${wanted.size} listing(s); slice core ${slice.core.size}, context ${slice.context.size}, " +
      s"${slice.http.size} answers (${slice.http.values.count(_ == SliceHttp.Outcome.Gap)} recording gaps); decided in " +
      f"${(System.nanoTime() - started) / 1e9}%.1fs at ${slice.recordedAt}")
    report(slice, wanted, decided, takes, labels)
    if (!replaying.gapsMet.isEmpty)
      println(s"UNANSWERED by the recording (read as unknown, as the leg reads them): ${replaying.gapsMet.asScala.map(_.render).toSeq.sorted.take(12).mkString("; ")}" +
        (if (replaying.gapsMet.size > 12) s" … ${replaying.gapsMet.size - 12} more" else ""))
    if (replaying.widened.nonEmpty) println(s"WIDENED from the recording: ${replaying.widened.size} answer(s) this ref asked that the slice lacked")
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

  /** What a pinned slice's listing must take, as labels.tsv judged it: `film` right for it, with the label's own note. */
  final case class Expected(venue: String, rawTitle: String, film: String, note: String)

  val ExpectedHeader = "venue\trawTitle\tfilm\tlabel"

  def readExpected(dir: Path): Seq[Expected] = Files.readAllLines(dir.resolve("expected.tsv"), StandardCharsets.UTF_8).asScala.toSeq.drop(1)
    .filter(_.nonEmpty).map(_.split("\t", -1)).map(a => Expected(a(0), a(1), a(2), a.lift(3).getOrElse("")))

  /** The right film labels.tsv names for `key` — its venue's own label first, then every venue's (`*`). */
  def rightLabel(key: ListingKey, country: Country, labels: Seq[UnmatchedClusters.Label]): Option[UnmatchedClusters.Label] = {
    val about = labels.filter(l => l.country == country.code && l.rawTitle == key.rawTitle && l.right)
    about.find(_.venue == key.venue).orElse(about.find(_.venue == "*"))
  }

  /** Copies `slice` (kept in `from`) to `target` with the film labels.tsv calls right for each seed — NEVER the take: a
   *  seed labels.tsv does not judge is left out, and a slice none of whose seeds it judges is refused. Each expectation is
   *  shown beside its label's own note, so a wrong label is seen before it is pinned. */
  def emitCase(slice: Slice, from: Path, labels: Seq[UnmatchedClusters.Label], target: Path): Unit = {
    val judged = slice.seeds.toSeq.sortBy(ListingKey.serialised).map(key => key -> rightLabel(key, slice.country, labels))
    val (known, unknown) = judged.partition(_._2.isDefined)
    unknown.foreach { case (key, _) => println(s"--emit-case: labels.tsv does not judge ${key.venue} | ${key.rawTitle} — not pinned (judge it first)") }
    if (known.isEmpty) println(s"--emit-case REFUSED for ${slice.selector}: labels.tsv judges none of its ${judged.size} listing(s); judge them first")
    else {
      Files.createDirectories(target)
      Seq("slice.json.gz", "rows.json.gz", "http.jsonl.gz").foreach(name => Files.copy(from.resolve(name), target.resolve(name), java.nio.file.StandardCopyOption.REPLACE_EXISTING))
      val lines = known.collect { case (key, Some(label)) => Seq(key.venue, key.rawTitle, label.film, s"${label.venue}: ${label.note}").mkString("\t") }
      Files.writeString(target.resolve("expected.tsv"), (ExpectedHeader +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
      println(s"\n--emit-case: ${known.size} listing(s) pinned in $target (IdentitySliceCasesSpec replays it):")
      known.collect { case (key, Some(label)) => println(s"  ${key.venue} | ${key.rawTitle} must take ${label.film} — label (${label.venue}): ${label.note}") }
    }
  }

  /** Each listing of a pinned slice and the film it takes now: what [[readExpected]] is held to. */
  def takesOf(dir: Path): Map[(String, String), String] = {
    val slice     = readSlice(dir)
    val replaying = new SliceHttp.Replaying(slice.http)
    val reads     = new Reads(slice.country, replaying, replaying.gaps, Clock.fixed(slice.recordedAt, ZoneOffset.UTC))
    val decided   = decide(slice.core, slice.context, reads, slice.selector)
    require(replaying.lacking.isEmpty, s"the slice $dir lacks answers: ${replaying.lacking.asScala.map(_.render).toSeq.sorted.take(5).mkString("; ")}")
    slice.seeds.toSeq.flatMap(key => decided.agreed.decisionOf.get(key).map(d => (key.venue, key.rawTitle) -> filmOf(d))).toMap
  }

  // ── faithfulness: a slice's take against the whole convergence leg's ────────────────────────

  def faithfulness(recording: Recording, opened: RecordedCorpus.Opened, index: IdentitySlicing.Index, takesPath: Path, o: Options,
                   labels: Seq[UnmatchedClusters.Label], out: Option[java.io.PrintWriter]): Unit = {
    val leg     = UnmatchedClusters.read(takesPath)
    val legOf   = leg.decisions.flatMap(d => d.members.map(_ -> d)).toMap
    val ours    = index.byKey
    // matched and unmatched alike, a fixed sample: half the leg's takes by the model, the rest its agreement's and its no-matches
    val rnd     = new scala.util.Random(o.seed)
    val inBoth  = leg.listings.map(_.key).filter(ours.contains)
    // every basis the leg decided by — model, agreement and no-match alike — an equal share, the rest topped up at random
    val byKind  = inBoth.groupBy(k => legOf.get(k).map(_.basis.toString).getOrElse("missing")).toSeq.sortBy(_._1)
      .map { case (kind, keys) => kind -> rnd.shuffle(keys.sortBy(ListingKey.serialised)) }
    val share   = math.max(1, o.sample / math.max(1, byKind.size))
    val even    = byKind.flatMap(_._2.take(share))
    val sample  = (even ++ rnd.shuffle(byKind.flatMap(_._2.drop(share))).take(math.max(0, o.sample - even.size))).take(o.sample)
    println(s"# faithfulness: ${sample.size} listing(s) of ${takesPath} (${leg.listings.size} listings; ${inBoth.size} in this recording) by the leg's basis: " +
      sample.groupBy(k => legOf.get(k).map(_.basis.toString).getOrElse("-")).map { case (b, ks) => s"$b ${ks.size}" }.toSeq.sorted.mkString(", "))
    var agree = 0
    var agreeWider = 0
    val misses = scala.collection.mutable.ListBuffer.empty[String]
    sample.zipWithIndex.foreach { case (key, i) =>
      val started   = System.nanoTime()
      val selection = IdentitySlicing.select(index, Set(key), o.widen)
      val core      = selection.core.flatMap(ours.get).toSeq.sortBy(_.sortKey)
      val context   = selection.context.flatMap(ours.get).toSeq.sortBy(_.sortKey)
      val decided   = decide(core, context, readsOf(opened, opened.fetch, opened.gaps, recording.recordedAt), s"faithfulness $i")
      val mine      = decided.agreed.decisionOf.get(key).map(filmOf).getOrElse("missing")
      val theirs    = legOf.get(key).map(filmOf).getOrElse("missing")
      val same      = mine == theirs
      if (same) agree += 1
      val legMembers = legOf.get(key).toSeq.flatMap(_.members).toSet
      val outside    = legMembers -- core.map(_.key)
      // a slice that differs is decided again one ring wider: what the next investigation of it should ask for
      def widened = {
        val wider = IdentitySlicing.select(index, Set(key), o.widen + 1)
        val again = decide(wider.core.flatMap(ours.get).toSeq.sortBy(_.sortKey), wider.context.flatMap(ours.get).toSeq.sortBy(_.sortKey),
          readsOf(opened, opened.fetch, opened.gaps, recording.recordedAt), s"faithfulness $i wider")
        val film = again.agreed.decisionOf.get(key).map(filmOf).getOrElse("missing")
        if (film == theirs) agreeWider += 1
        s"; --widen ${o.widen + 1} (core ${wider.core.size}): ${if (film == theirs) "SAME as the leg" else s"still $film"}"
      }
      val why = if (same) "" else (if (outside.nonEmpty) s"the leg's cluster holds ${outside.size} listing(s) outside the slice's core (e.g. ${outside.take(2).map(k => s"${k.venue} | ${k.rawTitle}").mkString("; ")})"
                else "same members — the slice lacks context the whole corpus gave it (a neighbouring family's facts or agreement)") + widened
      val line = f"${if (same) "SAME" else "DIFF"}\t${key.venue}\t${key.rawTitle}\tslice $mine (${decided.agreed.decisionOf.get(key).map(_.basis.toString).getOrElse("-")})\t" +
        f"leg $theirs (${legOf.get(key).map(_.basis.toString).getOrElse("-")})\tcore ${core.size} context ${context.size}\t${(System.nanoTime() - started) / 1e9}%.1fs\t$why"
      println(line)
      out.foreach(_.println(line))
      if (!same) misses += line
    }
    println(f"\n# faithfulness: $agree of ${sample.size} listings take what the whole leg took (${100.0 * agree / math.max(1, sample.size)}%.1f%%)")
    if (misses.nonEmpty) println(s"# slice ≠ whole corpus for ${misses.size}: one ring wider, $agreeWider of them take what the leg took (the DIFF lines above)")
  }
}
