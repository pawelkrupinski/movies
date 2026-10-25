package tools

import org.bson.BsonDocument
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import services.identity.{MongoPinStore, Pin}
import services.identity.adapters.TmdbKind

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._
import scala.util.Using

/**
 * What prod's identity read besides its listings and lookups, as a recording captures it beside its corpus
 * (`RecordCorpusFixture`, from the same read-only connection at the same moment): the operator pins (`identity_pins`) and
 * every family answer prod had filed (`identity_family_answers`). The live drift check (`scripts.IdentityLiveDrift`) builds
 * its whole resolve's inputs from them, as the worker builds its own (`ResolveInputs.of`), so the two sides of its
 * comparison read the same curation and the same answers. A recording from before them holds neither, and the drift
 * check refuses it rather than resolve unpinned over a capture's week-old answers.
 */
object ProdIdentityInputs {

  /** Prod's inputs at one moment. */
  final case class Captured(pins: Seq[Pin], families: Map[String, BsonDocument])

  def pinsPath(root: Path, code: String): Path     = root.resolve("corpus").resolve(s"prod-pins-$code.json")
  def familiesPath(root: Path, code: String): Path = root.resolve("corpus").resolve(s"prod-family-answers-$code.json.gz")
  /** Where a recording leg writes them: beside its corpus, packed into the same archive. */
  val Root: Path = Paths.get("test/resources/fixtures")

  /** How long the family answers' read may take, as the prod mirror's reads (`ProdMirror`): the recording step's own
   *  ten-minute ceiling cuts it off first. */
  val ReadTimeout: FiniteDuration = SpecTimeouts.Run

  /** Prod's pins and filed family answers, read from `database` (prod's country database, read-only). */
  def read(database: MongoDatabase): Captured = {
    val families = Await.result(database.getCollection[Document](TmdbKind.Family.collection).find().batchSize(MongoReplies.Default).toFuture(),
      ReadTimeout).map { d =>
      val doc = d.toBsonDocument
      val id  = doc.getString("_id").getValue
      doc.remove("_id")
      id -> doc
    }.toMap
    Captured(new MongoPinStore(Some(database)).all(), families)
  }

  /** Writes both files under `root`; the paths written. A pin is kept as `identity_pins` holds it. */
  def write(root: Path, code: String, captured: Captured): Seq[Path] = {
    val (pins, families) = (pinsPath(root, code), familiesPath(root, code))
    Files.createDirectories(pins.getParent)
    Files.writeString(pins, captured.pins.map(MongoPinStore.encode(_).toJson()).mkString("[", ",\n", "]\n"))
    Using.resource(new GZIPOutputStream(Files.newOutputStream(families))) { out =>
      val all = new BsonDocument()
      captured.families.toSeq.sortBy(_._1).foreach { case (id, doc) => all.append(id, doc) }
      out.write(all.toJson.getBytes(StandardCharsets.UTF_8))
    }
    Seq(pins, families)
  }

  /** What a recording laid out at `root` captured of prod's inputs; None for one recorded before it did. A pin that no
   *  longer decodes fails the read: the whole resolve would otherwise go without a pin the worker reads. */
  def readFrom(root: Path, code: String): Option[Captured] = {
    val (pins, families) = (pinsPath(root, code), familiesPath(root, code))
    Option.when(Files.exists(pins) && Files.exists(families)) {
      val docs = org.bson.BsonArray.parse(Files.readString(pins)).getValues.asScala.toSeq.map(_.asDocument)
      val decoded = docs.map(d => MongoPinStore.decode(Document(d)).getOrElse(sys.error(s"$pins: an undecodable pin ${d.get("_id")}")))
      val all = Using.resource(new GZIPInputStream(Files.newInputStream(families)))(in => BsonDocument.parse(new String(in.readAllBytes(), StandardCharsets.UTF_8)))
      Captured(decoded, all.keySet.asScala.toSeq.map(id => id -> all.getDocument(id)).toMap)
    }
  }
}
