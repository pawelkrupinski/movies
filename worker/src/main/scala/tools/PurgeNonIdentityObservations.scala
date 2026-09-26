package tools

import models.Country
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.model.{Filters, Projections, Sorts}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture}
import services.observations.{LookupQuery, ObservationStore}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One-shot cleanup of what the UNSCOPED observation capture filed: every `obs_lookups` row that
 * is not identity evidence (`LookupQuery.isIdentityEvidence` — a venue's detail or a TMDB request),
 * rating pages above all (docs/design/identity-resolver.md, "Capture"). The capture no longer
 * writes them; without this they would only age out a retention window later.
 *
 * DRY RUN by default: prints, per country, what would go (rows and keys by host) and deletes
 * nothing. `--apply` deletes, in bounded batches by key. Optional country codes (`de uk`) narrow
 * it; the default is every country. Against prod, over the SSH forward:
 *
 *   MONGODB_URI=<root uri> sbt "worker/runMain tools.PurgeNonIdentityObservations [--apply] [pl de …]"
 *
 * `def main`, not `extends App` — see `FilmwebDiff`.
 */
object PurgeNonIdentityObservations {

  enum Mode { case DryRun, Apply }

  final case class Request(mode: Mode, countries: Seq[Country])

  /** The command line, or why it is not one. */
  def parse(args: Seq[String]): Either[String, Request] = {
    val (flags, codes) = args.partition(_.startsWith("--"))
    val unknownFlags   = flags.filterNot(_ == "--apply")
    val countries      = codes.map(code => code -> Country.all.find(_.code.equalsIgnoreCase(code)))
    if (unknownFlags.nonEmpty) Left(s"unknown flag(s): ${unknownFlags.mkString(" ")}")
    else countries.collectFirst { case (code, None) => code } match {
      case Some(code) => Left(s"unknown country: $code (one of ${Country.all.map(_.code).mkString(" ")})")
      case None       => Right(Request(if (flags.contains("--apply")) Mode.Apply else Mode.DryRun,
                                       if (codes.isEmpty) Country.all else countries.flatMap(_._2)))
    }
  }

  /** The stored keys that are not identity evidence. */
  def doomed(keys: Seq[String]): Set[String] = keys.map(LookupQuery(_)).filterNot(_.isIdentityEvidence).map(_.key).toSet

  private val DefaultPageSize = 1000
  private val Timeout  = 60.seconds

  def main(args: Array[String]): Unit = parse(args.toSeq) match {
    case Left(problem) =>
      System.err.println(s"PurgeNonIdentityObservations: $problem")
      sys.exit(2)
    case Right(request) =>
      val address = settings.ProcessConfiguration.resolve().mongoAddress
      // MONGODB_DB pins ONE database for every country: each would purge the same one.
      if (address.database.isDefined && request.countries.size > 1) {
        System.err.println("PurgeNonIdentityObservations: MONGODB_DB names one database; unset it, or name one country")
        sys.exit(2)
      }
      request.countries.foreach { country =>
        val connection = new services.MongoConnection(address.uri, address.databaseFor(country), services.MongoRequirement.Required)
        try connection.database.foreach { db =>
          val keys = purge(db.getCollection[Document](ObservationStore.LookupsCollection), request.mode)
          val verb = if (request.mode == Mode.Apply) "deleted" else "would delete (dry run; --apply deletes)"
          println(s"${country.code}: $verb every version of ${keys.size} obs_lookups key(s): " +
            keys.toSeq.groupMapReduce(LookupQuery(_).host)(_ => 1)(_ + _).toSeq.sortBy { case (h, n) => (-n, h) }
              .map { case (h, n) => s"$h $n" }.mkString(", "))
        }
        finally connection.close()
      }
  }

  /** The non-evidence keys of `lookups`, deleted (every version) under `Apply`. Keyset-scans by `key`, deleting (under `Apply`) each page's non-evidence keys
   *  as it goes — a deleted key is behind the cursor, so the scan neither skips nor revisits one. */
  def purge(lookups: MongoCollection[Document], mode: Mode, pageSize: Int = DefaultPageSize): Set[String] = {
    val gone  = Set.newBuilder[String]
    var after = Option.empty[String]
    var more  = true
    while (more) {
      val page = Await.result(lookups.find(after.fold(Filters.empty())(Filters.gt("key", _)))
        .projection(Projections.include("key")).sort(Sorts.ascending("key")).limit(pageSize).toFuture(), Timeout)
      val keys  = page.flatMap(_.get[BsonString]("key").map(_.getValue))
      val found = doomed(keys)
      if (mode == Mode.Apply && found.nonEmpty)
        Await.result(lookups.deleteMany(Filters.in("key", found.toSeq*)).toFuture(), Timeout)
      gone ++= found
      after = keys.lastOption
      more  = page.sizeIs == pageSize
    }
    gone.result()
  }
}
