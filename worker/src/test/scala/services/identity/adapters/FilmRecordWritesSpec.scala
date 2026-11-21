package services.identity
package adapters

import services.identityadapters.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.SourceRecord
import services.identity.vocabulary.{Answer, VoterFamily}

/** Every store that keeps a film's record writes it in the ONE format ([[IdentityAnswerBson]]) and reads it back with
 *  every fact: a family's record answer, IMDb's record of a title (the agreed film: AgreementVerdictsSpec). */
class FilmRecordWritesSpec extends AnyFlatSpec with Matchers {
  private val roots = measures.Film("Roots", Some("Raíces"), Seq("Raices", "Wurzeln"), Some(1954), Some(83), Some(Seq("Benito Alazraki")),
    Some(Seq("MX")), Some(1.5), 47395, Some(java.time.LocalDate.of(1955, 4, 21)), Some("MXUS"), Seq(85, 80), Some("MX1955P"), 123,
    Some("US=PG"), Some("es"), 18, vocabulary.FilmCast.ofPairs(Seq(Seq(1, 2), Seq(3))))

  "a family's record answer" should "be filed in the one format and read back with every fact" in {
    val docs  = new InMemoryTmdbDocuments
    val store = new FamilyAnswerStore(docs, tools.SpecClock.Pinned)
    val found = SourceRecord(roots, Map("imdb" -> "tt0047395"))
    store.fileRecord(VoterFamily.Wiki, "Q7366720", Some(found))
    store.answers(VoterFamily.Wiki).record("Q7366720") shouldBe Answer.Known(Some(found))
    store.document(FamilyAnswerStore.recordId(VoterFamily.Wiki, "Q7366720")).map(_.get("record")) shouldBe Some(IdentityAnswerBson.film(Some(roots)))
  }

  "IMDb's record of a title" should "be filed in the one format and read back with every fact, none as null" in {
    val docs  = new InMemoryTmdbDocuments
    val store = new TmdbStore(docs, tools.SpecClock.Pinned)
    val id    = TmdbStore.imdbRecordId("tt0047395")
    store.imdbRecord(id, Some(roots))
    val record = docs.get(TmdbKind.Query, Seq(id)).get(id).map(_.get("record"))
    record shouldBe Some(IdentityAnswerBson.film(Some(roots)))
    record.flatMap(IdentityAnswerBson.filmOf) shouldBe Some(roots)
    store.imdbRecord(id, None)
    docs.get(TmdbKind.Query, Seq(id)).get(id).map(_.get("record")).flatMap(IdentityAnswerBson.filmOf) shouldBe None
  }
}
