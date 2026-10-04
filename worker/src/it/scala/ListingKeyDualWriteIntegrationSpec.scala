package integration

import tools.SpecTimeouts

import models.{CinemaShowing, KinoMuranow, Kinoteka, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies._
import tools._

import java.time.LocalDateTime
import scala.concurrent.Await

/**
 * Phase 4 of the identity migration (docs/design/identity-resolver.md): `movie_slots` and
 * `screenings` rows carry `listingKey` — the venue listing they belong to — written beside
 * today's `(filmId, slotKey)` by every path that creates one. Nothing reads it yet.
 *
 * Read off the RAW documents, against a real Mongo, because the field lives only there: each
 * repository write path on its own — the whole-record upsert, the per-slot patch, and a listing
 * key that moves under unchanged (and stripped) showtimes.
 *
 * `ListingKeyWritePathLintSpec` keeps a new write path from bypassing the stamp.
 */
class ListingKeyDualWriteIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite {

  private val at = LocalDateTime.of(2099, 3, 1, 18, 0)
  private def show(hour: Int) = Showtime(at.withHour(hour), None)

  private def raw(db: MongoDatabase, collection: String): Seq[org.bson.BsonDocument] =
    Await.result(db.getCollection[Document](collection).find().toFuture(), SpecTimeouts.Io).map(_.toBsonDocument)

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
      val repository = new MongoMovieRepository(Some(db), _root_.tools.SpecClock.Pinned, screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
      val (title, year) = ("Belle", Some(2013))
      val id = StoredMovieRecord.keyFor(title, year, titleNormalizer)
      def rowId(s: Source) = SlotKeyed.idOf(id, s.displayName)
      val record = MovieRecord(data = Map[Source, SourceData](paged -> pagedSlot, pageless -> pagelessSlot,
                                                              Tmdb -> SourceData(title = Some("Belle"), releaseYear = Some(2013))))

      // The whole-record write.
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

      // The venue corrects its own year: its listing's key moves while its showtimes do not. The
      // patch restamps BOTH rows — the screenings row too, because an ordinary re-scrape of a
      // resolved film reaches the store only as this patch, never as a whole-record upsert. And
      // it does so from the cache's STRIPPED records, which carry no showtimes: the restamp moves
      // the key alone and leaves the row's showtimes as they were.
      val corrected = moreShows.copy(data = moreShows.data + (pageless -> pagelessSlot.copy(releaseYear = Some(2014))))
      repository.updateIfPresent(title, year, ShowtimesDigest.stripForCache(moreShows), ShowtimesDigest.stripForCache(corrected)) shouldBe true
      val correctedKey = serialised(pagelessKey.copy(year = Some(2014)))
      keys(db, SlotsRepository.Collection)(rowId(pageless)) shouldBe correctedKey
      keys(db, ScreeningsRepository.Collection)(rowId(pageless)) shouldBe correctedKey
      screenings.findForFilm(id)(pageless.displayName) shouldBe pagelessSlot.showtimes
      // Idempotent: the same patch again finds the row already stamped and writes nothing.
      repository.updateIfPresent(title, year, ShowtimesDigest.stripForCache(moreShows), ShowtimesDigest.stripForCache(corrected)) shouldBe true
      keys(db, ScreeningsRepository.Collection)(rowId(pageless)) shouldBe correctedKey
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
          "verbosity" -> "queryPlanner")).toFuture(), SpecTimeouts.Io)
        val winning = explained.toBsonDocument.getDocument("queryPlanner").getDocument("winningPlan").toJson
        withClue(s"$collection: a read by listingKey must use the listingKey index (winning plan $winning): ") {
          winning should include("listingKey_1")
          winning should not include "COLLSCAN"
        }
      }
    }
  }
}
