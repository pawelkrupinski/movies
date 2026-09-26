package integration

import models.{CinemaShowing, KinoMuranow, Kinoteka, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies._
import tools._

import java.time.LocalDateTime
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Phase 4 of the identity migration (docs/design/identity-resolver.md): `movie_slots` and
 * `screenings` rows carry `listingKey` — the venue listing they belong to — written beside
 * today's `(filmId, slotKey)` by every path that creates or moves one. Nothing reads it yet.
 *
 * Read off the RAW documents, against a real Mongo, because the field lives only there:
 *
 *  - each repository write path on its own — the whole-record upsert (landing, the staging
 *    fold's completion, a re-key's retitle), the per-slot patch, a listing key that moves under
 *    unchanged showtimes, and the merge move (`SideCollectionMove`);
 *  - then the pipeline itself over the PL hard-cluster corpus, booted the way the convergence
 *    legs boot it (scrape, settle, canonicalise, staging fold, conclusion, projection): every row
 *    it leaves must carry the key its slot derives, whichever path wrote it.
 *
 * `ListingKeyWritePathLintSpec` keeps a new write path from bypassing the stamp.
 */
class ListingKeyDualWriteIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]
  override def afterAll(): Unit = { storages.foreach(s => Try(s.close())); super.afterAll() }

  /** The PL hard-cluster corpus booted through the whole pipeline, the way the convergence legs
   *  boot it (scrape, settle, canonicalise, staging fold, conclusion, projection) — once, for
   *  every test that reads what it leaves. */
  private lazy val plPipeline: MongoDatabase = {
    val corpus = IdentityShadow.hardClusters(Some(Set("pl"))).head
    IdentityShadow.bootPipeline(IdentityShadow.wiring(mongoTarget, corpus, storages, configuration.fixtureRoot, configuration.env))
    storages.last.connection.database.get
  }

  private val at = LocalDateTime.of(2099, 3, 1, 18, 0)
  private def show(hour: Int) = Showtime(at.withHour(hour), None)

  private def raw(db: MongoDatabase, collection: String): Seq[org.bson.BsonDocument] =
    Await.result(db.getCollection[Document](collection).find().toFuture(), 30.seconds).map(_.toBsonDocument)

  private def text(d: org.bson.BsonDocument, field: String): String = d.getString(field).getValue

  /** `_id -> listingKey` of every row of `collection`; `None` where the field is absent or null. */
  private def keys(db: MongoDatabase, collection: String): Map[String, Option[String]] =
    raw(db, collection).map(d => text(d, "_id") -> Option(d.get("listingKey")).filter(_.isString).map(_.asString.getValue)).toMap

  private def serialised(k: ListingKey): Option[String] = Some(ListingKey.serialised(k))

  private val paged     = CinemaShowing(KinoMuranow, "belle")
  private val pageless  = CinemaShowing(Kinoteka, "belle")
  private val pagedSlot = SourceData(title = Some("Belle"), rawTitle = Some("Belle (2013)"), filmUrl = Some("https://muranow.pl/belle"),
                                     releaseYear = Some(2013), showtimes = Seq(show(18)))
  private val pagelessSlot = SourceData(title = Some("Belle"), releaseYear = Some(2013), director = Seq("Amma Asante"),
                                        showtimes = Seq(show(20)))
  private val pagedKey    = ListingKey.Native(KinoMuranow.displayName, "https://muranow.pl/belle", "Belle (2013)")
  private val pagelessKey = ListingKey.Published(Kinoteka.displayName, "Belle", Some(2013), Seq("Amma Asante"))

  "every repository write path" should "stamp movie_slots and screenings with the row's listing key" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "listing-key-dual-write") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
      val (title, year) = ("Belle", Some(2013))
      val id = StoredMovieRecord.keyFor(title, year, titleNormalizer)
      def rowId(s: Source) = SlotKeyed.idOf(id, s.displayName)
      val record = MovieRecord(data = Map[Source, SourceData](paged -> pagedSlot, pageless -> pagelessSlot,
                                                              Tmdb -> SourceData(title = Some("Belle"), releaseYear = Some(2013))))

      // The whole-record write: the landing, a staging fold's completion, a re-key's retitle.
      repository.upsert(title, year, record)
      keys(db, SlotsRepository.Collection) shouldBe Map(
        rowId(paged) -> serialised(pagedKey), rowId(pageless) -> serialised(pagelessKey),
        rowId(Tmdb)  -> None)                                                   // an enrichment slot is no venue's listing
      keys(db, ScreeningsRepository.Collection) shouldBe Map(
        rowId(paged) -> serialised(pagedKey), rowId(pageless) -> serialised(pagelessKey))

      // The per-slot patch: new showtimes at one venue.
      val moreShows = record.copy(data = record.data + (paged -> pagedSlot.copy(showtimes = Seq(show(18), show(21)))))
      repository.updateIfPresent(title, year, record, moreShows) shouldBe true
      keys(db, ScreeningsRepository.Collection)(rowId(paged)) shouldBe serialised(pagedKey)

      // The venue corrects its own year: its listing's key moves while its showtimes do not.
      // The patch restamps the slot; the screenings row follows on the next whole-record write.
      val corrected = moreShows.copy(data = moreShows.data + (pageless -> pagelessSlot.copy(releaseYear = Some(2014))))
      repository.updateIfPresent(title, year, moreShows, corrected) shouldBe true
      val correctedKey = serialised(pagelessKey.copy(year = Some(2014)))
      keys(db, SlotsRepository.Collection)(rowId(pageless)) shouldBe correctedKey
      repository.upsert(title, year, corrected)
      keys(db, ScreeningsRepository.Collection)(rowId(pageless)) shouldBe correctedKey

      // The merge move: the rows change film, and keep the listing they belong to.
      val survivor = StoredMovieRecord.keyFor(title, Some(2014), titleNormalizer)
      repository.moveFilm(FilmId(id), FilmId(survivor)) shouldBe true
      keys(db, ScreeningsRepository.Collection) shouldBe Map(
        SlotKeyed.idOf(survivor, paged.displayName) -> serialised(pagedKey), SlotKeyed.idOf(survivor, pageless.displayName) -> correctedKey)
      keys(db, SlotsRepository.Collection).filter(_._1.startsWith(survivor)) shouldBe Map(
        SlotKeyed.idOf(survivor, paged.displayName) -> serialised(pagedKey), SlotKeyed.idOf(survivor, pageless.displayName) -> correctedKey,
        SlotKeyed.idOf(survivor, Tmdb.displayName)  -> None)
    }
  }

  "movie_slots and screenings" should "index listingKey, so a read by one listing is an index scan, not a collection scan" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "listing-key-index") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      slots.upsertSlot("belle|2013", paged.displayName, pagedSlot)
      screenings.upsertSlot("belle|2013", paged.displayName, ListedShowtimes(pagedSlot.showtimes, Some(pagedKey)))
      Seq(SlotsRepository.Collection, ScreeningsRepository.Collection).foreach { collection =>
        val explained = Await.result(db.runCommand(Document(
          "explain"   -> Document("find" -> collection, "filter" -> Document("listingKey" -> ListingKey.serialised(pagedKey))),
          "verbosity" -> "queryPlanner")).toFuture(), 30.seconds)
        val winning = explained.toBsonDocument.getDocument("queryPlanner").getDocument("winningPlan").toJson
        withClue(s"$collection: a read by listingKey must use the listingKey index (winning plan $winning): ") {
          winning should include("listingKey_1")
          winning should not include "COLLSCAN"
        }
      }
    }
  }

  "the pipeline" should "leave every side row it writes carrying the listing key of its slot (PL hard clusters)" in {
    val db      = plPipeline
    val slotDocs = raw(db, SlotsRepository.Collection)
    val slotKeyOf = slotDocs.map { d =>
      val slot = MovieCodecs.registry.get(classOf[SourceData]).decode(
        new org.bson.BsonDocumentReader(d.getDocument("slot")), org.bson.codecs.DecoderContext.builder().build())
      text(d, "_id") -> ListingKey.ofSlotRow(text(d, "slotKey"), slot).map(ListingKey.serialised)
    }.toMap
    val slotKeys       = keys(db, SlotsRepository.Collection)
    val screeningKeys  = keys(db, ScreeningsRepository.Collection)
    withClue(s"premise — the corpus must have landed venue rows: ${slotDocs.size} slots, ${screeningKeys.size} screenings: ") {
      slotKeyOf.values.count(_.isDefined) should be > 100
      screeningKeys.size should be > 100
    }
    withClue("movie_slots rows whose stored key is not the one their slot derives: ") {
      slotKeys.filter { case (rowId, stored) => stored != slotKeyOf(rowId) }.take(10) shouldBe empty
    }
    withClue("screenings rows whose stored key is not their slot's: ") {
      screeningKeys.filter { case (rowId, stored) => stored.isEmpty || !slotKeyOf.get(rowId).contains(stored) }.take(10) shouldBe empty
    }
  }

  "the shadow read" should "find every listing's rows by listingKey exactly as by slot key after the pipeline (PL hard clusters), and see a row the stamp missed" in {
    val db         = plPipeline
    val slots      = new MongoSlotsRepository(Some(db))
    val screenings = new MongoScreeningsRepository(Some(db))
    val registry   = new io.prometheus.metrics.model.registry.PrometheusRegistry()
    val shadow = new services.identity.ListingKeyShadowRead(slots, screenings, settings.ListingKeyShadowSample(Int.MaxValue),
      services.identity.ListingKeyShadowRead.gauge(registry), models.Country.Poland, new scala.util.Random(0))
    val unstamped = services.metrics.UnstampedListingCensus.gauge(registry)
    val census    = new services.metrics.UnstampedListingCensus(screenings, slots, unstamped, models.Country.Poland)

    val report = shadow.compare().get
    val venueSlots = keys(db, SlotsRepository.Collection).keys.count(id => ListingKey.isVenueRow(SlotKeyed.slotKeyOf(id)))
    withClue(s"every venue slot row is compared (${report.compared.size} of $venueSlots, ${report.unread} unread): ") {
      report.compared.size shouldBe venueSlots
      report.compared.size should be > 100
      report.unread shouldBe 0
    }
    info(s"shadow read over the PL hard clusters: ${report.count(services.identity.ListingKeyShadowRead.Agree)} of ${report.compared.size} listings agree")
    withClue("listings whose rows differ by listingKey from by slot key: ") {
      report.disagreements.take(10).map(_.describe) shouldBe empty
    }
    census.sample()
    unstamped.labelValues("pl", SlotsRepository.Collection).get() shouldBe 0.0
    unstamped.labelValues("pl", ScreeningsRepository.Collection).get() shouldBe 0.0

    // Teeth: one row of each collection loses its stamp, as a pre-phase-4 write would leave it.
    val sampled = report.compared.find(_.screeningsBySlotKey.nonEmpty).get.rowId
    Seq(SlotsRepository.Collection, ScreeningsRepository.Collection).foreach { collection =>
      Await.result(db.getCollection[Document](collection).updateOne(
        Document("_id" -> sampled), Document("$unset" -> Document("listingKey" -> ""))).toFuture(), 10.seconds)
    }
    val broken = shadow.compare().get
    broken.disagreements.map(d => (d.rowId, d.slotsAgree, d.screeningsAgree)) shouldBe Seq((sampled, false, false))
    census.sample()
    unstamped.labelValues("pl", SlotsRepository.Collection).get() shouldBe 1.0
    unstamped.labelValues("pl", ScreeningsRepository.Collection).get() shouldBe 1.0
  }
}
