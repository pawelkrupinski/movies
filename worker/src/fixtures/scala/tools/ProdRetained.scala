package tools

import org.bson.BsonDocument
import services.identity.adapters.{RetainedListingStore, MongoRetainedListingStore}
import services.identity.{Listing, ListingBson}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.jdk.CollectionConverters._
import scala.util.{Try, Using}

/**
 * The listings prod's identity model keeps for their facts alone (`identity_retained`, [[services.identity.ListingRetention]]):
 * no venue bills them now, a present listing of the country bills their title. Captured beside the corpus
 * (`RecordCorpusFixture`, the same connection), so every replay of the recording reads the model's input as prod's take-up
 * does -- the listings billed and these, under [[services.identity.ListingRetention.restored]] -- and none of them is
 * missing from a convergence row's input. One listing per line, as [[ListingBson]] writes it (its days included).
 */
object ProdRetained {

  def path(root: Path, code: String): Path = in(root.resolve("corpus"), code)
  /** The capture in a corpus directory (beside `cinema-scrapes-<cc>.json.gz`). */
  def in(corpusDir: Path, code: String): Path = corpusDir.resolve(s"prod-retained-$code.json.gz")

  /** What prod's worker kept, as its take-up reads it: the store, or -- never written -- the history's last held listings
   *  ([[RetainedListingStore.fromHistory]]). Captured as read: each replay keeps of it what the take-up's rule keeps
   *  against the listings billed ([[services.identity.ListingRetention.restored]]). */
  def read(database: org.mongodb.scala.MongoDatabase, now: Long): Seq[Listing] =
    new MongoRetainedListingStore(database).read().getOrElse {
      val history = new services.identity.adapters.MongoIdentityHistory(database, new services.TtlIndexMismatches)
      RetainedListingStore.fromHistory(history.since(Seq(services.identity.adapters.HistoryEntry.Listing),
        now - services.identity.adapters.IdentityHistory.Kept.toMillis).filter(_.at <= now))
    }

  def write(root: Path, code: String, listings: Seq[Listing]): Path = {
    val file = path(root, code)
    Files.createDirectories(file.getParent)
    Using.resource(new GZIPOutputStream(Files.newOutputStream(file)))(out =>
      listings.foreach(l => out.write((ListingBson.doc(l).toJson + "\n").getBytes(StandardCharsets.UTF_8))))
    file
  }

  /** What a recording laid out at `root` captured; empty for one recorded before it did (2026-11-01). A line naming a
   *  venue this build does not know is skipped, as the corpus skips its rows. */
  def readFrom(root: Path, code: String): Seq[Listing] = readIn(root.resolve("corpus"), code)

  /** [[readFrom]] a corpus directory. */
  def readIn(corpusDir: Path, code: String): Seq[Listing] =
    Option(in(corpusDir, code)).filter(Files.exists(_)).toSeq.flatMap { file =>
      Using.resource(new java.io.BufferedReader(new java.io.InputStreamReader(new GZIPInputStream(Files.newInputStream(file)), StandardCharsets.UTF_8)))(
        _.lines().iterator().asScala.filter(_.nonEmpty).flatMap(line => Try(ListingBson.of(BsonDocument.parse(line))).toOption).toVector)
    }
}
