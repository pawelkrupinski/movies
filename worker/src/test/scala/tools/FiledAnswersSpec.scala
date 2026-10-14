package tools

import models.Country
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._

import java.nio.file.Files

/** The answers a recording does not hold as an identity slice reads them ([[FiledAnswers]]): prod's family answers
 *  served first, the capture's live TMDB answers only where the recording has none, every one read noted — never one
 *  the slice filed itself from the recording — and kept on disk as read. */
class FiledAnswersSpec extends AnyFlatSpec with Matchers {

  private val uk = Country.all.find(_.code == "uk").get
  private def doc(answer: String) = new BsonDocument("answer", new BsonString(answer))
  private val treasure = Hit(1710881, "Treasure Island", None, Some(2027), 5.0)
  private val filed = FiledAnswers(FiledAnswers.of(uk).capture.copy(
    families = Map("imdb|title|Treasure Island" -> doc("tt1"), "rt|title|Treasure Island" -> doc("none")),
    finds = Map("tt1" -> Some(1710881)), queries = Map(CandidateQuery.Title("Treasure Island") -> Seq(treasure))), "uk.json.gz")

  /** A recording answering one search and gapping every other question. */
  private val recording = new IdentityLookups {
    def hasDetail(l: Listing): Boolean = false
    def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Unknown
    def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (q == CandidateQuery.Title("Heat")) Answer.Known(Nil) else Answer.Unknown
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }

  "Prod's filed family answers" should "be served, and note exactly those read" in {
    val docs = new FiledAnswers.Noting(filed)
    docs.get(TmdbKind.Family, Seq("imdb|title|Treasure Island", "wiki|title|Treasure Island")) shouldBe Map("imdb|title|Treasure Island" -> doc("tt1"))
    docs.read.keySet.toArray.toSeq shouldBe Seq("imdb|title|Treasure Island")
  }

  they should "not count an answer filed over one of them as prod's" in {
    val docs = new FiledAnswers.Noting(filed)
    docs.put(TmdbKind.Family, Seq("rt|title|Treasure Island" -> doc("filed from the recording")))
    docs.get(TmdbKind.Family, Seq("rt|title|Treasure Island"))
    docs.read.isEmpty shouldBe true
  }

  "The capture's live answers" should "answer only what the recording does not, each read noted" in {
    val lookups = new FiledAnswers.Lookups(recording, filed)
    lookups.candidates(CandidateQuery.Title("Heat")) shouldBe Answer.Known(Nil)
    lookups.candidates(CandidateQuery.Title("Treasure Island")) shouldBe Answer.Known(Seq(treasure))
    lookups.candidates(CandidateQuery.Title("Nowhere")) shouldBe Answer.Unknown
    lookups.queries.keySet.toArray.toSeq shouldBe Seq(CandidateQuery.Title("Treasure Island"))
  }

  "What a slice read" should "read back from disk as written, its source named" in {
    val lookups = new FiledAnswers.Lookups(recording, filed)
    lookups.candidates(CandidateQuery.Title("Treasure Island"))
    val read = FiledAnswers.read(filed, Map("imdb|title|Treasure Island" -> doc("tt1")), Map("tt1" -> Some(1710881)), Some(lookups))
    val dir  = Files.createTempDirectory("filed")
    FiledAnswers.write(dir, read)
    val back = FiledAnswers.readFrom(dir, uk)
    back.source shouldBe "uk.json.gz"
    back.families shouldBe read.families
    back.finds shouldBe read.finds
    back.capture.queries shouldBe Map(CandidateQuery.Title("Treasure Island") -> Seq(treasure))
    FiledAnswers.readFrom(Files.createTempDirectory("none"), uk).isEmpty shouldBe true
  }
}
