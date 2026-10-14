package tools

import models.Country
import org.bson.BsonDocument
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoDatabase, ObservableFuture}
import services.identity._
import services.identity.agreement.{AgreementStage, MongoAgreementVerdicts, StoredVerdict}
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** One listing as prod's stored state holds it: the model's stored decision on it (`identity_model_families`), its trace
 *  (`identity_traces`), the agreement stage's stored verdict on its cluster and its stored correction
 *  (`identity_agreements`), and the stored titles of the films they name (`tmdb_films`). */
final case class ProdSide(listing: ListingKey, model: Option[ProdSide.Model], trace: Option[ListingTrace], verdict: Option[StoredVerdict],
                          correction: Option[StoredVerdict], titles: Map[Int, String]) {
  /** The film prod's stored state takes for the listing (`tmdb:N`, `imdb:tt…`, `none`, or `missing` when prod holds no
   *  record of it) and the basis: a correction over the model's take, else the model's film, else the families' agreed
   *  film, else the fill's — as the agreement stage applies its verdicts. */
  def take: (String, String) = model match {
    case None => ("missing", "-")
    case Some(m) =>
      correction.flatMap(_.correction).flatMap(_.outcome).map(o => (o.film.fold("none")(id => s"tmdb:$id"), "Corrected"))
        .orElse(m.film.map(id => (s"tmdb:$id", m.basis)))
        .orElse(verdict.flatMap(_.agreed).map(a => (a.crossId("tmdb").map(id => s"tmdb:$id").orElse(a.crossId("imdb").map(id => s"imdb:$id"))
          .getOrElse(s"${a.record.film.title}"), s"Agreed ${a.families.map(_.label).toSeq.sorted.mkString(",")}")))
        .orElse(verdict.flatMap(_.filled).map(f => (f.tmdb.map(id => s"tmdb:$id").orElse(f.imdb.map(id => s"imdb:$id")).getOrElse("none"), s"Filled ${f.rule}")))
        .getOrElse(("none", m.basis))
  }

  def render: Seq[String] = {
    def titled(film: Option[Int]) = film.fold("none")(id => s"tmdb:$id${titles.get(id).fold("")(t => s" «$t»")}")
    val (film, basis) = take
    Seq(s"PROD ${listing.venue} | ${listing.rawTitle}: $film via $basis${film.stripPrefix("tmdb:").toIntOption.flatMap(titles.get).fold("")(t => s" «$t»")}") ++
      model.map(m => s"  prod model: ${titled(m.film)} (${m.basis}), cluster of ${m.members.size}") ++
      trace.map(t => s"  prod trace: rules ${if (t.rules.isEmpty) "-" else t.rules.mkString(" ")}${t.blocker.fold("")(b => s"; blocker $b")}" +
        s"${t.weighedFilm.filterNot(t.film.contains).fold("")(w => s"; weighed tmdb:$w")}") ++
      verdict.map(v => s"  prod agreement: ${v.agreed.fold("no agreement")(a => s"agreed ${a.families.map(_.label).toSeq.sorted.mkString(",")} on '${a.record.film.title}'")}" +
        s"${v.filled.fold("")(f => s"; filled by ${f.rule}: ${f.line}")} (rules ${v.rules})") ++
      correction.flatMap(_.correction).map(c => s"  prod correction: ${c.line.getOrElse("corrects nothing")}${if (c.waiting) " (waiting on an answer)" else ""}") ++
      Option.when(model.isEmpty)("  prod holds no model family for it: not listed in prod now, or keyed otherwise")
  }
}

object ProdSide {
  /** The model's stored decision on a listing: its film, basis and the cluster's members. */
  final case class Model(film: Option[Int], basis: String, members: Seq[ListingKey])

  /** The id the agreement stage files a cluster's verdict under: its smallest listing ([[StoredFamily.idOf]]). */
  def clusterId(members: Seq[ListingKey]): String = StoredFamily.idOf(members)
}

/** What a slice reads of prod's state beside its recording: the stage's stored verdicts, the operator pins, and each
 *  listing's prod side. `none` is the hermetic slice: no prod state at all. */
final case class ProdState(name: String, verdicts: Seq[StoredVerdict], pins: Seq[Pin], sides: Seq[ListingKey] => Map[ListingKey, ProdSide])

object ProdState {
  val none: ProdState = ProdState("", Nil, Nil, _ => Map.empty)
}

/**
 * Prod's identity state as the LOCAL prod mirror holds it (scripts/local-mirror: `<prod db>_prod_mirror` on :28017, kept
 * by change-stream tailers) — never prod itself: a URI that is not the loopback mirror is refused. Read-only.
 */
final class ProdMirror(client: MongoClient, val country: Country) extends AutoCloseable {
  val database: String = services.MongoConnection.mirrorDbFor(country.mongoDb)
  private val db: MongoDatabase = client.getDatabase(database)
  private val Timeout = 120.seconds
  private def find(collection: String, filter: org.bson.conversions.Bson): Seq[BsonDocument] =
    Await.result(db.getCollection[Document](collection).find(filter).batchSize(MongoReplies.Default).toFuture(), Timeout).map(_.toBsonDocument)

  /** Every family answer prod filed (`identity_family_answers`), as a capture holds them: the document without its id. */
  def familyAnswers(): Map[String, BsonDocument] =
    find(TmdbKind.Family.collection, Filters.empty()).map { d =>
      val id = d.getString("_id").getValue
      d.remove("_id")
      id -> d
    }.toMap

  def verdicts(): Seq[StoredVerdict] = new MongoAgreementVerdicts(db).all()
  def pins(): Seq[Pin] = new MongoPinStore(Some(db)).all()

  /** Each listing's prod side. */
  def sides(keys: Seq[ListingKey]): Map[ListingKey, ProdSide] = if (keys.isEmpty) Map.empty else {
    val traces = find(MongoIdentityTraceStore.Collection, Filters.in("_id", keys.map(ListingKey.serialised)*))
      .map(MongoIdentityTraceStore.decode).map(t => t.listing -> t).toMap
    val families = find(MongoIdentityModelStore.FamiliesCollection, Filters.in("_id", traces.values.map(_.family).toSeq.distinct*))
    // decoded as the model store wrote them (its members bucketed by venue): ResolverDecisionBson reads either layout
    val models = families.flatMap(f => Option(f.get("decisions")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala).map(_.asDocument))
      .map(ResolverDecisionBson.decode).flatMap { d =>
        val model = ProdSide.Model(d.film, d.basis.toString, d.members)
        d.members.map(_ -> model)
      }.toMap
    val clusters = keys.flatMap(models.get).map(m => ProdSide.clusterId(m.members)).distinct
    val stored = if (clusters.isEmpty) Map.empty[String, StoredVerdict]
      else find(services.identity.agreement.AgreementVerdicts.Collection, Filters.in("_id", (clusters ++ clusters.map(AgreementStage.correctionId))*))
        .map(services.identity.agreement.AgreementVerdicts.decode).map(v => v.id -> v).toMap
    val named = (models.values.flatMap(_.film) ++ stored.values.flatMap(v => v.filled.flatMap(_.tmdb) ++ v.correction.flatMap(_.film))).toSeq.distinct
    val titles = if (named.isEmpty) Map.empty[Int, String]
      else find(TmdbKind.Film.collection, Filters.in("_id", named.map(_.toString)*)).flatMap { d =>
        Option(d.get("record")).filter(_.isDocument).map(_.asDocument).flatMap(r => Option(r.get("title")).filter(_.isString))
          .map(t => d.getString("_id").getValue.toInt -> t.asString.getValue)
      }.toMap
    keys.map { key =>
      val model   = models.get(key)
      val cluster = model.map(m => ProdSide.clusterId(m.members))
      key -> ProdSide(key, model, traces.get(key), cluster.flatMap(stored.get), cluster.map(AgreementStage.correctionId).flatMap(stored.get), titles)
    }.toMap
  }

  def state(): ProdState = ProdState(database, verdicts(), pins(), sides)

  def close(): Unit = client.close()
}

object ProdMirror {
  /** The mirror at `uri` — the local loopback one only: a reader of prod's state goes through the mirror, never to prod. */
  def open(uri: String, country: Country): ProdMirror = {
    val host = scala.util.Try(new java.net.URI(uri.replaceFirst("^mongodb://", "http://")).getHost).getOrElse("")
    require(Set("127.0.0.1", "localhost", "::1")(host), s"--prod-mirror reads only the LOCAL mirror (127.0.0.1), never prod: $uri")
    new ProdMirror(MongoClient(uri), country)
  }
}
