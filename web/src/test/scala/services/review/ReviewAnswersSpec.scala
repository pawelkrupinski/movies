package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.Instant

/** The answer history's meaning (shared by every store), the import of the hand-built pages' answers
 *  (run against a copy of the user's 71), and the answer the review page posts. */
class ReviewAnswersSpec extends AnyFlatSpec with Matchers {

  private val at = Instant.parse("2026-10-06T08:00:00Z")
  private val m1 = ReviewMember("Kino A", "Film", Some("https://a/film"))
  private val m2 = ReviewMember("Kino B", "Film", None)
  private def answer(verdict: ReviewVerdict, members: Seq[ReviewMember] = Seq(m1), clusterId: Option[String] = None) =
    ReviewAnswer(clusterId.getOrElse(ReviewClusterId.of(members)), "pl", ReviewPage.Queue, verdict, None,
      Some(FilmFacts(FilmRef.tmdb(1))), "Film", members, "dev", at)

  "the current answer" should "be the latest for a cluster, and none once that is an undo" in {
    ReviewAnswers.current(Seq(answer(ReviewVerdict.Right), answer(ReviewVerdict.Wrong))).map(_.verdict) shouldBe Seq(ReviewVerdict.Wrong)
    ReviewAnswers.current(Seq(answer(ReviewVerdict.Right), answer(ReviewVerdict.Undo))) shouldBe empty
    ReviewAnswers.current(Seq(answer(ReviewVerdict.Right), answer(ReviewVerdict.Undo), answer(ReviewVerdict.Event)))
      .map(_.verdict) shouldBe Seq(ReviewVerdict.Event)
  }

  it should "cover a cluster that has since gained a listing" in {
    val index = new ReviewAnswers.Index(ReviewAnswers.current(Seq(answer(ReviewVerdict.Event))))
    index.answerFor(ReviewClusterId.of(Seq(m1, m2)), Seq(m1, m2)).map(_.verdict) shouldBe Some(ReviewVerdict.Event)
    index.answerFor(ReviewClusterId.of(Seq(m2)), Seq(m2)) shouldBe None
  }

  "importing" should "add each answer once, however often it runs" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val first   = answer(ReviewVerdict.Right).copy(legacyId = Some("a"))
    answers.importAll(Seq(first)) shouldBe 1
    answers.importAll(Seq(first)) shouldBe 0
    answers.history() should have size 1
  }

  "the hand-built review pages' answers" should "import whole, the film each card showed kept" in {
    val json   = new String(Files.readAllBytes(Paths.get(getClass.getResource("/review/all-answers.json").toURI)), StandardCharsets.UTF_8)
    val parsed = ReviewImport.parse(json)
    parsed should have size 71
    parsed.groupBy(a => (a.page, a.verdict)).view.mapValues(_.size).toMap shouldBe Map(
      (ReviewPage.Matchable, ReviewVerdict.Right) -> 36, (ReviewPage.Queue, ReviewVerdict.NoneOfThese) -> 8,
      (ReviewPage.Queue, ReviewVerdict.Film) -> 7, (ReviewPage.Matchable, ReviewVerdict.Event) -> 5,
      (ReviewPage.Queue, ReviewVerdict.Event) -> 4, (ReviewPage.Queue, ReviewVerdict.Bill) -> 4,
      (ReviewPage.Matchable, ReviewVerdict.Wrong) -> 4, (ReviewPage.Matchable, ReviewVerdict.Film) -> 2,
      (ReviewPage.Recent, ReviewVerdict.Bill) -> 1)
    parsed.forall(_.legacyId.isDefined) shouldBe true

    val streetcar = parsed.find(_.legacyId.contains("a3d2a998c98ae2c6")).get
    streetcar.shown.map(_.ref.render) shouldBe Some("tmdb:291289")
    streetcar.members.map(_.venue) shouldBe Seq("Prince Charles London")
    val macbeth = parsed.find(_.legacyId.contains("458ee358a5f7de86")).get
    macbeth.ref.map(_.render) shouldBe Some("tmdb:1703622")
    val lotr = parsed.find(_.legacyId.contains("67a6f0aebd11b498")).get
    lotr.shown.map(_.ref.render) shouldBe Some("tmdb:122")                  // recently matched: the matched film
    val kafka = parsed.find(_.legacyId.contains("54db3d68e38702ef")).get
    kafka.shown.map(_.ref.render) shouldBe Some("filmweb:10008278")         // matchable: the labelled film

    // every answer becomes rows (or says why not), and its export is idempotent
    val (rows, _) = LabelsExport.merge(Nil, parsed)
    LabelsExport.merge(rows, parsed)._1 shouldBe rows
  }

  "a posted answer" should "carry a warning when the chosen film contradicts the venue's own facts" in {
    val card = Json.obj("clusterId" -> "c1", "country" -> "pl", "page" -> "matchable", "title" -> "Queen",
      "members" -> Json.arr(Json.obj("venue" -> "Planken", "rawTitle" -> "Queen", "page" -> "https://p/q", "year" -> 2020, "directors" -> Json.arr())),
      "shown" -> Json.obj("ref" -> "tmdb:519465", "title" -> "Queen of Hearts", "year" -> 2019, "directors" -> Json.arr("May el-Toukhy")),
      "films" -> Json.arr(Json.obj("ref" -> "tmdb:1", "title" -> "Queen Rock Montreal", "year" -> 1981, "directors" -> Json.arr())))
    val right = AnswerRequest.parse(Json.obj("card" -> card, "verdict" -> "right"), "dev", at).toOption.get
    right.warnings shouldBe empty                                           // 2020 vs 2019: one year either side is no contradiction
    val other = AnswerRequest.parse(Json.obj("card" -> card, "verdict" -> "film", "ref" -> "https://www.themoviedb.org/movie/1-queen"), "dev", at)
      .toOption.get
    other.ref shouldBe Some(FilmRef.tmdb(1))
    other.warnings shouldBe Seq("Planken states Queen is from 2020; Queen Rock Montreal (1981) is from 1981")
    AnswerRequest.parse(Json.obj("card" -> card, "verdict" -> "film", "ref" -> "not a link"), "dev", at).isLeft shouldBe true
    AnswerRequest.parse(Json.obj("card" -> card, "verdict" -> "maybe"), "dev", at).isLeft shouldBe true
  }
}
