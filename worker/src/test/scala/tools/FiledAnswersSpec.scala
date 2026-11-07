package tools

import models.Country
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.identity.adapters.TmdbKind


/** The answers a recording does not hold as an identity slice reads them ([[FiledAnswers]]): prod's family answers
 *  served first, the capture's live TMDB answers only where the recording has none, every one read noted — never one
 *  the slice filed itself from the recording — and kept on disk as read. */
class FiledAnswersSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private val uk = Country.all.find(_.code == "uk").get
  private def doc(answer: String) = new BsonDocument("answer", new BsonString(answer))
  private val treasure = Hit(1710881, "Treasure Island", None, Some(2027), 5.0)
  private val filed = FiledAnswers(FiledAnswers.of(uk).capture.copy(
    families = Map("imdb|title|Treasure Island" -> doc("tt1"), "rt|title|Treasure Island" -> doc("none")),
    finds = Map("tt1" -> Some(1710881)), queries = Map(CandidateQuery.Title("Treasure Island") -> Seq(treasure))), "uk.json.gz")

  /** A recording answering one search and gapping every other question. */
  private val recording = new KnownSearchesLookups(Map(CandidateQuery.Title("Heat") -> Nil))

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

  "The capture's live HTTP answers" should "answer a request the recording lacks, before its gap, and leave the rest to it" in {
    val url  = "https://v3.sg.media-imdb.com/suggestion/t/treasure%20island.json"
    val gap  = new HttpFetch {
      def get(u: String): String = s"gap $u"
      def post(u: String, body: String, contentType: String): String = s"gap $u $body"
    }
    val leaf = new FiledAnswers.LiveLeaf(Map(s"GET $url" -> """{"d":[]}""", "POST https://x.example/q {}" -> "posted"), gap)
    leaf.get(url) shouldBe """{"d":[]}"""
    leaf.get(url, Map("Accept" -> "application/json")) shouldBe """{"d":[]}"""
    leaf.post("https://x.example/q", "{}", "application/json") shouldBe "posted"
    leaf.get("https://elsewhere.example/") shouldBe "gap https://elsewhere.example/"
    leaf.served shouldBe 3
  }

  they should "be kept in the capture's file, and read back as written" in {
    val capture = filed.capture.copy(live = Map("GET https://v3.sg.media-imdb.com/suggestion/t/treasure.json" -> """{"d":[]}"""))
    val file    = tempDirectory("capture").resolve("uk.json.gz")
    UnmatchedClusters.write(file, capture)
    UnmatchedClusters.read(file).live shouldBe capture.live
    // read off the file by FiledAnswers itself, which identity-slice drops into refs whose Capture has no `live`
    FiledAnswers.ofCapture(file).live shouldBe capture.live
    FiledAnswers(FiledAnswers.of(uk).capture, "uk.json.gz", capture.live).isEmpty shouldBe false
  }

  "What a slice read" should "read back from disk as written, its source named" in {
    val lookups = new FiledAnswers.Lookups(recording, filed)
    lookups.candidates(CandidateQuery.Title("Treasure Island"))
    val read = FiledAnswers.read(filed, Map("imdb|title|Treasure Island" -> doc("tt1")), Map("tt1" -> Some(1710881)), Some(lookups))
    val dir  = tempDirectory("filed")
    FiledAnswers.write(dir, read)
    val back = FiledAnswers.readFrom(dir, uk)
    back.source shouldBe "uk.json.gz"
    back.families shouldBe read.families
    back.finds shouldBe read.finds
    back.capture.queries shouldBe Map(CandidateQuery.Title("Treasure Island") -> Seq(treasure))
    FiledAnswers.readFrom(tempDirectory("none"), uk).isEmpty shouldBe true
  }

  "Prod's own family answers" should "stand over the capture's, the capture's live HTTP answers kept beneath them" in {
    val live      = filed.copy(live = Map("GET https://v3.sg.media-imdb.com/suggestion/t/treasure%20island.json" -> "{}"))
    val preferred = FiledAnswers.preferring(live, Map("rt|title|Treasure Island" -> doc("tt1")), "prod mirror")
    preferred.families("rt|title|Treasure Island") shouldBe doc("tt1")
    preferred.families("imdb|title|Treasure Island") shouldBe doc("tt1")
    preferred.live shouldBe live.live
    preferred.source shouldBe "prod mirror (over uk.json.gz)"
  }
}

