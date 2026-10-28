package tools

import models.{CinemaMovie, Country}
import org.bson.BsonDocument
import org.bson.json.{JsonMode, JsonWriterSettings}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import services.identity._
import services.identity.adapters.{LiveAsks, MongoTmdbDocuments, QuestionMemory, StoredFirstLookups, StoredTmdbLookups, TmdbDocuments, TmdbKind,
  TmdbStore, VenueDetailLookups, VenuePageIndex}
import services.movies.TitleNormalizer
import services.venuepages.CapturedVenuePages

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Clock
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.concurrent.Await
import scala.jdk.CollectionConverters._
import scala.util.Using

/**
 * What prod's identity model answers its questions from, as a recording captures it beside its corpus
 * (`RecordCorpusFixture`, from the same read-only connection, moments after the corpus): the TMDB store's documents
 * (`tmdb_films`, `tmdb_people`, `tmdb_queries`), the question memory's markers (`identity_question_markers`) and the
 * venue pages (`venue_pages`) of the recorded listings. The worker answers its model from these first and asks TMDB live
 * only on a gap its memory says is due (`WorkerWiring.cutoverLookups`); the live drift check (`scripts.IdentityLiveDrift`)
 * answers its whole resolve through the same composition ([[lookups]]), the recording's live answers standing in for the
 * worker's live asks.
 *
 * Which documents: every one a resolve of the recorded listings reads off prod's store — the model over prod's pins,
 * through [[lookups]] over prod's documents with every live ask refused, so nothing is fetched and nothing written to
 * prod ([[Capturing]]). An answer, and the whole document, are kept apart: a film's answer is a projection of its
 * document (`TmdbKind.answerFields`), and a document the agreement stage alone reads whole (a candidate's cast, its
 * overview) is not captured — the drift check reads the recording's for it, as the worker would ask TMDB.
 */
object ProdLookupInputs {

  /** Prod's documents as a resolve read them: each kind's answers and whole documents by id, and the venue pages. */
  final case class Captured(answers: Map[TmdbKind, Map[String, BsonDocument]], whole: Map[TmdbKind, Map[String, BsonDocument]],
                            pages: Seq[BsonDocument]) {
    def render: String = s"${count(answers)} TMDB answer(s), ${count(whole)} whole document(s), ${pages.size} venue page(s)"
    private def count(docs: Map[TmdbKind, Map[String, BsonDocument]]) = docs.values.map(_.size).sum
  }

  def path(root: Path, code: String): Path = root.resolve("corpus").resolve(s"prod-lookups-$code.json.gz")
  /** Where a recording leg writes it: beside its corpus, packed into the same archive. */
  val Root: Path = ProdIdentityInputs.Root

  /** How long the capture may take before the recorder gives up on it: inside the recording step's ten-minute ceiling,
   *  after the corpus read's two, so a slow capture costs the drift check this recording, never the corpus. */
  val ReadTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(5, "minutes")

  /** The worker's lookups over `docs` and `pages` (`WorkerWiring.cutoverLookups`): the TMDB store's answers, venue_pages
   *  for a listing's detail, and on a gap the question memory over `docs` deciding whether `live` is asked. */
  def lookups(docs: TmdbDocuments, pages: Seq[BsonDocument], enrichers: Seq[services.cinemas.common.DetailEnricher], live: IdentityLookups,
              country: Country, clock: Clock): IdentityLookups = {
    val language = country.language.toLanguageTag
    val details  = new VenueDetailLookups(enrichers, new VenuePageIndex(CapturedVenuePages.store(pages)), ObservationReads.Untracked)
    new StoredFirstLookups(new StoredTmdbLookups(new TmdbStore(docs, clock), language, details, ObservationReads.Untracked), live,
      LiveAsks.remembered(new QuestionMemory(docs, language, clock)))
  }

  /** What prod's model reads of `database` (prod's country database, read-only) to resolve `byCinema`. */
  def read(database: MongoDatabase, byCinema: Seq[(models.Cinema, Seq[CinemaMovie])], country: Country, clock: Clock): Captured = {
    val listings = ModelRead.corpus(byCinema, TitleNormalizer.forCountry(country))
    capture(new MongoTmdbDocuments(database), venuePages(database, listings.flatMap(_.page).distinct), listings,
      new MongoPinStore(Some(database)).all(), country, clock)
  }

  /** What a resolve of `listings` over `pins` reads of `prod`'s documents through [[lookups]], every live ask refused: the
   *  documents it read, never one it was not asked for, and nothing written to `prod`. */
  def capture(prod: TmdbDocuments, pages: Seq[BsonDocument], listings: Seq[Listing], pins: Seq[Pin], country: Country, clock: Clock): Captured = {
    val docs = new Capturing(prod)
    IdentityResolver.resolve(listings, lookups(docs, pages, enrichersOf(country, clock), Refused, country, clock), TitleNormalizer.forCountry(country),
      ResolveInputs.of(pins, services.identity.TakeJudgements.Index.bundled(country.code)))
    docs.captured(pages)
  }

  /** The venue detail enrichers of `country`'s venues: which detail group each venue's pages are filed under. A page is
   *  read from venue_pages alone ([[lookups]]); none is ever fetched ([[NoFetch]]). */
  def enrichersOf(country: Country, clock: Clock): Seq[services.cinemas.common.DetailEnricher] = {
    val catalog = new services.cinemas.CinemaScraperCatalog(NoFetch, new models.VenueClock(clock), clock, TitleNormalizer.forCountry(country))
    country.cities.flatMap(city => catalog.byCity.getOrElse(city, Nil)).collect { case e: services.cinemas.common.DetailEnricher => e }.distinct
  }

  private def venuePages(database: MongoDatabase, pages: Seq[String]): Seq[BsonDocument] =
    pages.grouped(MongoReplies.Default).toSeq.flatMap { batch =>
      Await.result(database.getCollection[BsonDocument](services.venuepages.MongoVenuePageStore.Collection).find(Filters.in("page", batch*))
        .batchSize(MongoReplies.Default).toFuture(), ReadTimeout)
    }

  /** Every live ask refused: a gap stays the gap prod's store holds. */
  private object Refused extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                       = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]      = Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]        = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]  = Answer.Unknown
  }

  private object NoFetch extends HttpFetch {
    private def refused(url: String) = throw new IllegalStateException(s"the capture reads venue_pages, never a venue: $url")
    def get(url: String): String                                       = refused(url)
    def post(url: String, body: String, contentType: String): String   = refused(url)
  }

  /** `inner`'s documents, each read once and kept as read — an answer apart from a whole document; a write is kept here
   *  and never reaches `inner` (the question memory marks what the refused live asks left unanswered). */
  final class Capturing(inner: TmdbDocuments) extends TmdbDocuments {
    private def perKind[A]       = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, A]()).toMap
    private val answered         = perKind[Option[BsonDocument]]
    private val wholes           = perKind[Option[BsonDocument]]
    private val written          = perKind[BsonDocument]

    private def through(held: ConcurrentHashMap[String, Option[BsonDocument]], kind: TmdbKind, ids: Seq[String])(
        read: Seq[String] => Map[String, BsonDocument]): Map[String, BsonDocument] = {
      val (own, theirs) = ids.distinct.partition(written(kind).containsKey)
      val missing       = theirs.filterNot(held.containsKey)
      if (missing.nonEmpty) { val got = read(missing); missing.foreach(id => held.put(id, got.get(id))) }
      own.map(id => id -> written(kind).get(id).clone()).toMap ++ theirs.flatMap(id => held.get(id).map(id -> _.clone()))
    }
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = through(wholes(kind), kind, ids)(inner.get(kind, _))
    override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = through(answered(kind), kind, ids)(inner.answers(kind, _))
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = docs.foreach { case (id, d) => written(kind).put(id, d.clone()) }

    /** What was read of `inner`, with `pages`. */
    def captured(pages: Seq[BsonDocument]): Captured = {
      def found(held: Map[TmdbKind, ConcurrentHashMap[String, Option[BsonDocument]]]) =
        held.map { case (kind, docs) => kind -> docs.asScala.collect { case (id, Some(d)) => id -> d }.toMap }.filter(_._2.nonEmpty)
      Captured(found(answered), found(wholes), pages)
    }
  }

  /** A capture's documents, as prod's store held them: an answer read as captured, else projected from a whole document
   *  held; a write (the question memory's) kept beside them. */
  final class Held(captured: Captured) extends TmdbDocuments {
    private val wholes = TmdbKind.values.map(kind => kind -> new ConcurrentHashMap[String, BsonDocument](captured.whole.getOrElse(kind, Map.empty).asJava)).toMap
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
      ids.flatMap(id => Option(wholes(kind).get(id)).map(id -> _.clone())).toMap
    override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      val (whole, rest) = ids.distinct.partition(wholes(kind).containsKey)
      super.answers(kind, whole) ++ rest.flatMap(id => captured.answers.get(kind).flatMap(_.get(id)).map(id -> _.clone()))
    }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = docs.foreach { case (id, d) => wholes(kind).put(id, d.clone()) }
  }

  private val Exact = JsonWriterSettings.builder().outputMode(JsonMode.EXTENDED).build()

  /** Writes the capture under `root`; the path written. */
  def write(root: Path, code: String, captured: Captured): Path = {
    val file = path(root, code)
    Files.createDirectories(file.getParent)
    def byKind(docs: Map[TmdbKind, Map[String, BsonDocument]]) = {
      val all = new BsonDocument()
      docs.toSeq.sortBy(_._1.collection).foreach { case (kind, byId) =>
        val ofKind = new BsonDocument()
        byId.toSeq.sortBy(_._1).foreach { case (id, d) => ofKind.append(id, d) }
        all.append(kind.collection, ofKind)
      }
      all
    }
    val doc = new BsonDocument("answers", byKind(captured.answers)).append("whole", byKind(captured.whole))
      .append("pages", new org.bson.BsonArray(captured.pages.asJava))
    Using.resource(new GZIPOutputStream(Files.newOutputStream(file)))(_.write(doc.toJson(Exact).getBytes(StandardCharsets.UTF_8)))
    file
  }

  /** What a recording laid out at `root` captured of prod's lookups; None for one recorded before it did. */
  def readFrom(root: Path, code: String): Option[Captured] = Option(path(root, code)).filter(Files.exists(_)).map { file =>
    val doc = Using.resource(new GZIPInputStream(Files.newInputStream(file)))(in => BsonDocument.parse(new String(in.readAllBytes(), StandardCharsets.UTF_8)))
    def byKind(all: BsonDocument) = TmdbKind.values.toSeq.filter(kind => all.containsKey(kind.collection)).map { kind =>
      val ofKind = all.getDocument(kind.collection)
      kind -> ofKind.keySet.asScala.toSeq.map(id => id -> ofKind.getDocument(id)).toMap
    }.toMap
    Captured(byKind(doc.getDocument("answers")), byKind(doc.getDocument("whole")), doc.getArray("pages").getValues.asScala.toSeq.map(_.asDocument))
  }
}
