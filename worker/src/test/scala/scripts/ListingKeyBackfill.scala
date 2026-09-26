package scripts

import models.{Country, SourceData}
import org.bson.codecs.DecoderContext
import org.bson.{BsonDocument, BsonDocumentReader}
import org.mongodb.scala.model.{BulkWriteOptions, Filters, Projections, Sorts, Updates}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}
import services.MongoConnection
import services.movies.{ListingKey, MovieCodecs, ScreeningsRepository, SlotsRepository}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One-shot backfill for phase 4 of the identity migration (docs/design/identity-resolver.md):
 * stamp `listingKey` on every EXISTING `movie_slots` and `screenings` row. Rows written since the
 * dual write shipped already carry it (`StoredSlotDto.of`, `ScreeningsSplit.screeningsOf`); this
 * reaches the rows no write has touched since.
 *
 * The key is [[ListingKey.ofSlotRow]] over the stored slot — the same derivation the write path
 * stamps with, so a backfilled row and a freshly written one cannot differ. A `screenings` row
 * takes the key of the `movie_slots` row at the same `_id` (its slot); one with no such slot, or
 * whose slot is no venue's listing, is counted and left alone.
 *
 * The dry run also reports:
 *  - COLLISIONS: listing keys more than one slot row claims. Across films that is one venue
 *    listing filed on two films (or two listings the key cannot tell apart — the Belle / Planet
 *    of the Apes class, which `ListingKeyCorpusSpec` proves the key separates on every recorded
 *    corpus); within one film, a legacy bare-cinema slot beside its per-title successor.
 *  - with `--export <dir>`: each film's set of listing keys (`films-<cc>.json`) — the "previous
 *    assignment" ID seeding runs `IdAssigner` against (`IdentitySeedingIntegrationSpec`) — and
 *    the full collision list (`collisions-<cc>.txt`).
 *
 * ==Running it==
 *
 * RUN IT ONLY AFTER THE DUAL WRITE IS DEPLOYED: a pre-phase-4 worker rewrites a row whole and
 * drops the field again. Each write is conditional on the row still holding the key the scan
 * read (absent, or a stale one), so a row the live worker rewrote since is skipped, never
 * clobbered. Every stamped row rings its collection's change stream, so each film it touches is
 * re-projected once (the projection is idempotent; nothing it serves changes) — run it one
 * country at a time.
 *
 * Each country is its own database, taken from `Country.mongoDb` (never `MONGODB_DB`, which
 * `.env.local` pins to Poland). DRY RUN BY DEFAULT; `--apply` writes.
 *
 * {{{
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel
 *   sbt "worker/Test/runMain scripts.ListingKeyBackfill"                          # dry run, all countries
 *   sbt "worker/Test/runMain scripts.ListingKeyBackfill --export /tmp/lk es"      # dry run + export, Spain
 *   sbt "worker/Test/runMain scripts.ListingKeyBackfill --apply es"               # WRITE, Spain
 * }}}
 *
 * Mongo only, no external service: the scan pages by `_id` (never one unbounded cursor, which
 * overflowed the driver's stack on these collections before — `KeysetScan`), and the writes go
 * in unordered bulks of [[WriteBatch]]. Throughput is printed per country.
 */
object ListingKeyBackfill {

  /** A `movie_slots` row as the scan reads it: the slot's listing fields and its stored key. */
  final case class SlotRow(id: String, filmId: String, slotKey: String, slot: SourceData, stored: Option[String])
  /** A `screenings` row as the scan reads it — its showtimes are not needed. */
  final case class ScreeningRow(id: String, filmId: String, slotKey: String, stored: Option[String])
  /** Set row `id`'s `listingKey` to `listingKey`, provided it still holds `stored`. */
  final case class Update(id: String, stored: Option[String], listingKey: String)
  /** A listing key several slot rows claim: `(filmId, slotKey)` of each. */
  final case class Collision(listingKey: String, rows: Seq[(String, String)]) {
    def acrossFilms: Boolean = rows.map(_._1).distinct.sizeIs > 1
  }

  final case class Plan(slotUpdates: Seq[Update], screeningUpdates: Seq[Update],
                        slotsWithoutListing: Int, screeningsWithoutListing: Seq[String],
                        collisions: Seq[Collision], filmListings: Map[String, Set[String]])

  /** The whole backfill as a value: pure, and the same order whatever order the rows came in. */
  def plan(slots: Seq[SlotRow], screenings: Seq[ScreeningRow]): Plan = {
    val derived: Map[String, Option[String]] =
      slots.iterator.map(r => r.id -> ListingKey.ofSlotRow(r.slotKey, r.slot).map(ListingKey.serialised)).toMap
    def updates[R](rows: Seq[R], id: R => String, stored: R => Option[String], key: R => Option[String]): Seq[Update] =
      rows.flatMap(r => key(r).filterNot(k => stored(r).contains(k)).map(Update(id(r), stored(r), _))).sortBy(_.id)
    val keyed = slots.flatMap(r => derived(r.id).map(k => (k, r)))
    Plan(
      slotUpdates              = updates[SlotRow](slots, _.id, _.stored, r => derived(r.id)),
      screeningUpdates         = updates[ScreeningRow](screenings, _.id, _.stored, r => derived.get(r.id).flatten),
      slotsWithoutListing      = slots.count(r => derived(r.id).isEmpty),
      screeningsWithoutListing = screenings.filter(r => derived.get(r.id).flatten.isEmpty).map(_.id).sorted,
      collisions = keyed.groupMap(_._1)(_._2).toSeq.collect {
        case (k, rows) if rows.sizeIs > 1 => Collision(k, rows.map(r => r.filmId -> r.slotKey).sorted)
      }.sortBy(_.listingKey),
      filmListings = keyed.groupMapReduce(_._2.filmId)(kr => Set(kr._1))(_ ++ _))
  }

  /** Rows per bulk write. */
  private val WriteBatch = 500
  /** Rows per scan page. */
  private val ScanPage = 2000

  def main(args: Array[String]): Unit = {
    val apply     = args.contains("--apply")
    val exportDir = args.sliding(2).collectFirst { case Array("--export", dir) => Paths.get(dir) }
    val requested = args.filterNot(a => a.startsWith("--") || exportDir.exists(_.toString == a)).toSeq
    val countries =
      if (requested.isEmpty) Country.all
      else requested.map(code => Country.byCode(code).getOrElse {
        println(s"Unknown country code '$code' — expected one of ${Country.all.map(_.code).mkString(", ")}."); sys.exit(1)
      })
    println(if (apply) "APPLY — rows whose listingKey is missing or stale will be WRITTEN."
            else "DRY RUN — nothing is written. Pass --apply to write.")
    exportDir.foreach(Files.createDirectories(_))
    countries.foreach(backfill(_, apply, exportDir))
    sys.exit(0)
  }

  private def backfill(country: Country, apply: Boolean, exportDir: Option[Path]): Unit = {
    val process    = _root_.settings.ProcessConfiguration.resolve()
    val connection = MongoConnection.forCountry(country,
      process.mongoAddress.copy(database = Some(_root_.settings.MongoDatabaseName(country.mongoDb))),
      required = services.MongoRequirement.Required, services.MongoTuning.from(process))
    val database = connection.database.getOrElse {
      println(s"${country.displayName}: could not open ${country.mongoDb} — is the tunnel up and MONGODB_URI set?"); sys.exit(1)
    }
    val started = System.nanoTime()
    val slots = scan(database.getCollection[Document](SlotsRepository.Collection),
      Projections.include("filmId", "slotKey", "listingKey", "slot.title", "slot.rawTitle", "slot.releaseYear",
        "slot.director", "slot.filmUrl")) { d =>
      SlotRow(text(d, "_id"), text(d, "filmId"), text(d, "slotKey"), slotOf(d), stored(d))
    }
    val screenings = scan(database.getCollection[Document](ScreeningsRepository.Collection),
      Projections.include("filmId", "slotKey", "listingKey")) { d =>
      ScreeningRow(text(d, "_id"), text(d, "filmId"), text(d, "slotKey"), stored(d))
    }
    val p = plan(slots, screenings)
    val across = p.collisions.count(_.acrossFilms)
    println(f"${country.displayName}%-15s movie_slots ${slots.size} rows: ${p.slotUpdates.size} to stamp " +
      s"(${p.slotUpdates.count(_.stored.isDefined)} stale), ${p.slotsWithoutListing} name no venue listing · " +
      s"screenings ${screenings.size} rows: ${p.screeningUpdates.size} to stamp, ${p.screeningsWithoutListing.size} without a keyed slot · " +
      s"collisions ${p.collisions.size} (${across} across films, ${p.collisions.size - across} within one) · ${p.filmListings.size} films")
    p.screeningsWithoutListing.take(10).foreach(id => println(s"    screenings row with no keyed slot: ${readable(id)}"))
    p.collisions.sortBy(c => (!c.acrossFilms, c.listingKey)).take(20).foreach { c =>
      println(s"    ${if (c.acrossFilms) "ACROSS" else "within"} ${readable(c.listingKey)}: ${c.rows.map { case (f, s) => s"$f @ $s" }.mkString("; ")}")
    }
    exportDir.foreach { dir =>
      val films = JsObject(p.filmListings.toSeq.sortBy(_._1).map { case (f, ks) => f -> JsArray(ks.toSeq.sorted.map(JsString(_))) })
      write(dir.resolve(s"films-${country.code}.json"), Json.stringify(JsObject(Seq("country" -> JsString(country.code), "films" -> films))))
      write(dir.resolve(s"collisions-${country.code}.txt"), p.collisions.map { c =>
        s"${if (c.acrossFilms) "ACROSS" else "within"}\t${readable(c.listingKey)}\t${c.rows.map { case (f, s) => s"$f @ $s" }.mkString("; ")}"
      }.mkString("", "\n", "\n"))
    }
    if (apply) {
      val written = stamp(database, SlotsRepository.Collection, p.slotUpdates) + stamp(database, ScreeningsRepository.Collection, p.screeningUpdates)
      println(s"    wrote $written of ${p.slotUpdates.size + p.screeningUpdates.size} (the rest changed since the scan and were skipped)")
    }
    val seconds = (System.nanoTime() - started) / 1e9
    println(f"    ${slots.size + screenings.size} rows in $seconds%.1fs (${(slots.size + screenings.size) / math.max(seconds, 0.001)}%.0f rows/s)")
    connection.close()
  }

  /** A key or row id as a person reads it: the NUL and unit separators shown as ` | `. */
  def readable(key: String): String = key.replace("\u0000", " | ").replace(services.movies.SlotKeyed.IdSep.toString, " | ")

  private def text(d: BsonDocument, field: String): String = d.getString(field).getValue

  private def stored(d: BsonDocument): Option[String] = Option(d.get("listingKey")).filter(_.isString).map(_.asString.getValue)

  private val slotCodec = MovieCodecs.registry.get(classOf[SourceData])

  private def slotOf(d: BsonDocument): SourceData =
    Option(d.get("slot")).filter(_.isDocument).fold(SourceData())(s =>
      slotCodec.decode(new BsonDocumentReader(s.asDocument), DecoderContext.builder().build()))

  /** Every document of `c`, `_id`-keyset paged, projected, decoded by `row`. */
  private def scan[R](c: MongoCollection[Document], projection: org.bson.conversions.Bson)(row: BsonDocument => R): Vector[R] = {
    val out = Vector.newBuilder[R]
    var after = Option.empty[String]
    var more  = true
    while (more) {
      val page = Await.result(c.find(after.fold(Filters.empty())(Filters.gt("_id", _))).projection(projection)
        .sort(Sorts.ascending("_id")).limit(ScanPage).toFuture(), 120.seconds).map(_.toBsonDocument)
      page.foreach(d => out += row(d))
      after = page.lastOption.map(text(_, "_id"))
      more  = page.sizeIs == ScanPage
    }
    out.result()
  }

  /** Conditional `$set` per update, in unordered bulks; returns how many rows it changed. */
  private def stamp(database: MongoDatabase, collection: String, updates: Seq[Update]): Long = {
    val c = database.getCollection[Document](collection)
    updates.grouped(WriteBatch).map { batch =>
      val models = batch.map { u =>
        val unchanged = u.stored.fold(Filters.or(Filters.exists("listingKey", false), Filters.eq("listingKey", null)))(Filters.eq("listingKey", _))
        new com.mongodb.client.model.UpdateOneModel[Document](Filters.and(Filters.eq("_id", u.id), unchanged), Updates.set("listingKey", u.listingKey))
      }
      Await.result(c.bulkWrite(models, BulkWriteOptions().ordered(false)).toFuture(), 120.seconds).getModifiedCount.toLong
    }.sum
  }

  private def write(path: Path, text: String): Unit = { Files.writeString(path, text, StandardCharsets.UTF_8); () }
}
