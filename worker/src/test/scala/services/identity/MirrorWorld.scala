package services.identity

import models.Country
import org.bson.BsonDocument
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import services.identity.adapters.{IdentityHistoryReader, IdentityHistoryReplay, MongoIdentityHistory, MongoTmdbDocuments, TmdbDocuments}
import services.movies.TitleNormalizer

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Await

/**
 * The local prod mirror's world at a moment (docs/design/identity-structural-fixes.md, fix 3: one replay world): the
 * listings billed then, the venue pages and the TMDB and family documents as they stood then, read back through the
 * identity history ([[IdentityHistoryReplay]]) -- and the CLOCK at that moment, never the harness's own. A replay of
 * 2026-11-02 read at a wall clock of 2026-11-06 dated its year, its release windows and its venues' "today" by the
 * day it ran (the same harness bug as verify's dating of kept listings, a57790c723f2). What `MirrorRecompute`,
 * `FamilyCounterfactual` and `identity-what-if` read the mirror through.
 */
object MirrorWorld {

  /** The mirror at `clock`'s instant: its listings (sorted, one per key), venue pages and TMDB / family documents. */
  final case class Moment(listings: Seq[Listing], pages: Seq[BsonDocument], documents: TmdbDocuments, clock: Clock)

  /** The world `history` replays to at `at`, over the listings billed now (`billed`), the pages and the documents now. */
  def of(history: IdentityHistoryReader, billed: Seq[Listing], pages: Seq[BsonDocument], documents: TmdbDocuments, at: Instant): Moment = {
    val replay = IdentityHistoryReplay.from(history, at.toEpochMilli)
    Moment(replay.listings(billed).sorted.distinctBy(_.key), replay.pages(pages), replay.documents(documents), Clock.fixed(at, ZoneOffset.UTC))
  }

  /** The mirror `db` of `country` at `at`. */
  def at(db: MongoDatabase, country: Country, at: Instant, normalizer: TitleNormalizer): Moment = {
    val rows  = tools.ProdListings.read(db, tools.CountryScrapeCorpus.cinemasOf(country).toSet)
    val pages = Await.result(db.getCollection[BsonDocument](services.venuepages.MongoVenuePageStore.Collection).find()
      .batchSize(tools.MongoReplies.Default).toFuture(), tools.SpecTimeouts.Run)
    of(new MongoIdentityHistory(db, new services.TtlIndexMismatches), ModelRead.corpus(rows.map(r => r.cinema -> r.films), normalizer),
      pages, new MongoTmdbDocuments(db), at)
  }
}
