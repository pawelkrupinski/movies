package services.observations

import models.{Cinema, CinemaMovie}
import play.api.libs.json._
import services.cinemas.common.CinemaMovieJson
import services.movies.ListingKey

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.{Clock, Instant}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.concurrent.duration.FiniteDuration

/**
 * The identity program's evidence store: every scraped listing and every external lookup, as
 * immutable, timestamped observations (docs/design/identity-resolver.md, "Phase 1:
 * observations"). Written by the shadow capture (`ObservingHttpFetch`, `ObservingDetailEnricher`,
 * `ObservingScrapeArchive`) and read by nothing that serves.
 *
 * Every rule lives HERE, above the storage seam, so the Mongo and in-memory backends cannot
 * disagree about any of them:
 *
 *  - an observation is NEW only when its content differs from the key's current one; the same
 *    answer seen again just restamps `lastSeenAt`. Content is never rewritten;
 *  - a transient failure (timeout, 5xx, 429) never supersedes a definitive answer — a failed
 *    read is not data — but is kept when nothing better is known, so the question is on record;
 *  - expiry is [[ObservationRetention]]'s one rule: a current observation lives a window past
 *    its last observation or READ, a superseded one a window past its replacement. The store
 *    stamps `expireAt`; Mongo's TTL monitor deletes, and every read here filters by the same
 *    stamp so a backend's deletion lag is never visible.
 *
 * One writer per key at a time (a striped lock): a worker is the only writer of its country's
 * collections.
 */
final class ObservationStore(
  listings: ObservationBackend,
  lookups:  ObservationBackend,
  clock:    Clock,
  window:   FiniteDuration = ObservationRetention.Window
) {
  import ObservationStore._

  // ── writes ────────────────────────────────────────────────────────────────

  def observeListing(cinema: Cinema, listing: CinemaMovie): Unit = {
    val evidence = ListingObservation.evidence(listing)
    record(listings, ListingKey.serialised(ListingKey.of(cinema, listing)), cinema.displayName,
      CinemaMovieJson.encode(Seq(evidence)), replaces = _ => true)
  }

  def observeLookup(query: LookupQuery, answer: LookupAnswer): Unit =
    record(lookups, query.key, query.host, Json.stringify(answerJson(answer)),
      replaces = current => answer.definitive || !decodeAnswer(current).definitive)

  private def record(backend: ObservationBackend, key: String, scope: String, content: String,
                     replaces: StoredObservation => Boolean): Unit = lockFor(key).synchronized {
    val now     = clock.instant()
    val hash    = sha256(content)
    val expires = now.plusMillis(window.toMillis)
    val stored  = backend.current(key)
    stored.filter(live(now)) match {
      case Some(current) if current.hash == hash => backend.renew(key, Some(now), expires)
      case Some(current) if !replaces(current)   => ()
      case _ =>
        // An expired current row may still be physically present (TTL lag): it is retired too,
        // at its own expiry, so a key never has two current observations and nothing expired
        // comes back to life.
        stored.foreach(c => backend.retire(key, if (live(now)(c)) expires else c.expireAt))
        backend.insert(StoredObservation(key, scope, hash, gzip(content), now, now, expires, current = true))
    }
  }

  // ── reads ─────────────────────────────────────────────────────────────────

  /** The current answer to `query`, if one is live. A READ renews it: a lookup something still
   *  reads is evidence in use, whether or not anything re-fetched it. */
  def lookup(query: LookupQuery): Option[LookupObservation] = lockFor(query.key).synchronized {
    val now = clock.instant()
    lookups.current(query.key).filter(live(now)).map { o =>
      lookups.renew(query.key, None, now.plusMillis(window.toMillis))
      toLookup(o)
    }
  }

  def lookupHistory(query: LookupQuery): Seq[LookupObservation] =
    lookups.history(query.key).filter(live(clock.instant())).sortBy(_.observedAt).map(toLookup)

  def listing(key: ListingKey): Option[ListingObservation] =
    listings.current(ListingKey.serialised(key)).filter(live(clock.instant())).flatMap(toListing)

  def listingHistory(key: ListingKey): Seq[ListingObservation] =
    listings.history(ListingKey.serialised(key)).filter(live(clock.instant())).sortBy(_.observedAt).flatMap(toListing)

  /** Every live listing — the resolver's listing set. */
  def currentListings(): Seq[ListingObservation] =
    listings.allCurrent().filter(live(clock.instant())).flatMap(toListing)

  /** Every live lookup answer. Not a read in the renewing sense: nothing consumes them here. */
  def currentLookups(): Seq[LookupObservation] =
    lookups.allCurrent().filter(live(clock.instant())).map(toLookup)

  def close(): Unit = { listings.close(); lookups.close() }

  private def live(now: Instant)(o: StoredObservation): Boolean = o.expireAt.isAfter(now)

  private def toLookup(o: StoredObservation): LookupObservation =
    LookupObservation(LookupQuery(o.key), decodeAnswer(o), o.observedAt, o.lastSeenAt)

  /** `None` for a row whose venue left the catalogue — its listing belongs to no cinema. */
  private def toListing(o: StoredObservation): Option[ListingObservation] =
    Cinema.byDisplayName.get(o.scope).flatMap { cinema =>
      CinemaMovieJson.decode(gunzip(o.payload), cinema).headOption
        .map(cm => ListingObservation(ListingKey.of(cinema, cm), cinema, cm, o.observedAt, o.lastSeenAt))
    }

  private val locks = Array.fill(64)(new Object)
  private def lockFor(key: String): Object = locks(Math.floorMod(key.hashCode, locks.length))
}

object ObservationStore {

  /** The shadow collections. Nothing serving reads them. */
  val ListingsCollection = "obs_listings"
  val LookupsCollection  = "obs_lookups"

  def inMemory(clock: Clock): ObservationStore =
    new ObservationStore(new InMemoryObservationBackend, new InMemoryObservationBackend, clock)

  private[observations] def answerJson(answer: LookupAnswer): JsObject = answer match {
    case LookupAnswer.Body(text)                    => Json.obj("body" -> text)
    case LookupAnswer.Bytes(base64)                 => Json.obj("bytes" -> base64)
    case LookupAnswer.Failed(status, method, message) =>
      Json.obj("method" -> method, "message" -> message) ++ status.fold(Json.obj())(s => Json.obj("status" -> s))
  }

  private def decodeAnswer(o: StoredObservation): LookupAnswer = {
    val js = Json.parse(gunzip(o.payload))
    (js \ "body").asOpt[String].map(LookupAnswer.Body.apply)
      .orElse((js \ "bytes").asOpt[String].map(LookupAnswer.Bytes.apply))
      .getOrElse(LookupAnswer.Failed((js \ "status").asOpt[Int], (js \ "method").as[String], (js \ "message").as[String]))
  }

  private def sha256(content: String): String =
    MessageDigest.getInstance("SHA-256").digest(content.getBytes(StandardCharsets.UTF_8)).map("%02x".format(_)).mkString

  private def gzip(content: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(out)
    try gz.write(content.getBytes(StandardCharsets.UTF_8)) finally gz.close()
    out.toByteArray
  }

  private def gunzip(bytes: Array[Byte]): String = {
    val in = new GZIPInputStream(new java.io.ByteArrayInputStream(bytes))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  }
}
