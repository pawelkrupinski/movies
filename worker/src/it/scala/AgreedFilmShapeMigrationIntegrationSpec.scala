package integration

import tools.SpecTimeouts

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityAnswerBson
import services.identity.agreement.{AgreedFilm, AgreementVerdicts, MongoAgreementVerdicts, SourceRecord, StoredVerdict}
import services.identity.measures.Film
import services.identity.vocabulary.{FilmCast, VoterFamily}
import services.identityadapters.AgreedFilmShapeMigration
import tools.IsolatedMongoDatabase

import scala.concurrent.Await

/** The agreed films `identity_agreements` kept in the earlier format are rewritten, through a real Mongo, into the one
 *  format: read back as they were, none left, a second run a no-op, a verdict in the one format untouched. */
class AgreedFilmShapeMigrationIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val roots = Film("Roots", Some("Raíces"), Nil, Some(1954), Some(83), Some(Seq("Benito Alazraki")), Some(Seq("MX")), Some(1.5), 47395,
    cast = FilmCast.ofPairs(Seq(Seq(1, 2), Seq(3))))
  private def verdict(id: String, film: Film) = StoredVerdict(id, 7L, Map.empty, Some(AgreedFilm(Set(VoterFamily.Imdb),
    SourceRecord(film, Map("imdb" -> "tt0047395")), Map(VoterFamily.Imdb -> "tt0047395"))))

  /** `film` as FilmBson wrote it: the cast under `cast`, no empty `alternativeTitles`. */
  private def earlier(film: Film): BsonDocument = {
    val d = IdentityAnswerBson.film(Some(film)).asDocument.clone()
    if (film.alternativeTitles.isEmpty) d.remove("alternativeTitles")
    Option(d.remove("castPairs")).foreach(d.append("cast", _))
    d
  }

  "the agreed-film migration" should "rewrite every earlier-shape film in batches, read back as it was, and find none on a second run" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "agreed-film-shape") { db =>
      val collection = db.getCollection[Document](AgreementVerdicts.Collection)
      val films  = (1 to 7).map(i => f"v$i%02d" -> roots.copy(title = s"Roots $i"))
      val stored = films.map { case (id, film) =>
        val d = AgreementVerdicts.encode(verdict(id, film)); d.getDocument("agreed").put("film", earlier(film)); Document(d)
      }
      val current = AgreementVerdicts.encode(verdict("w", roots))
      Await.result(collection.insertMany(stored :+ Document(current)).toFuture(), SpecTimeouts.Io)

      val migration = new AgreedFilmShapeMigration(db, batch = 3)
      migration.left() shouldBe 7L
      migration.run() shouldBe AgreedFilmShapeMigration.Outcome(before = 7, rewritten = 7, unreadable = 0, left = 0)
      val read = new MongoAgreementVerdicts(db).all().map(v => v.id -> v.agreed.map(_.record.film)).toMap
      read shouldBe (films.map { case (id, film) => id -> Some(film) } :+ ("w" -> Some(roots))).toMap
      withClue("every film now in the one format: ") {
        Await.result(collection.find().toFuture(), SpecTimeouts.Io).map(_.toBsonDocument.getDocument("agreed").getDocument("film"))
          .foreach(film => IdentityAnswerBson.film(IdentityAnswerBson.filmOf(film)) shouldBe film)
      }
      migration.run() shouldBe AgreedFilmShapeMigration.Outcome(before = 0, rewritten = 0, unreadable = 0, left = 0)
    }
  }

  it should "stop at its bound, leave what it could not read counted, and finish on the next run" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "agreed-film-shape-bound") { db =>
      val collection = db.getCollection[Document](AgreementVerdicts.Collection)
      val films = (1 to 5).map(i => f"v$i%02d" -> roots.copy(title = s"Roots $i"))
      val unreadable = new BsonDocument("cast", new BsonArray(java.util.List.of(new BsonArray(java.util.List.of(BsonInt32(1))))))
        .append("year", BsonString("not a year"))
      val docs = films.map { case (id, film) =>
        val d = AgreementVerdicts.encode(verdict(id, film)); d.getDocument("agreed").put("film", earlier(film)); Document(d)
      } :+ { val d = AgreementVerdicts.encode(verdict("v00", roots)); d.getDocument("agreed").put("film", unreadable); Document(d) }
      Await.result(collection.insertMany(docs).toFuture(), SpecTimeouts.Io)

      val bounded = new AgreedFilmShapeMigration(db, batch = 2, maxBatches = 1)
      bounded.run() shouldBe AgreedFilmShapeMigration.Outcome(before = 6, rewritten = 1, unreadable = 1, left = 5)
      new AgreedFilmShapeMigration(db, batch = 2).run() shouldBe AgreedFilmShapeMigration.Outcome(before = 5, rewritten = 4, unreadable = 1, left = 1)
    }
  }
}
