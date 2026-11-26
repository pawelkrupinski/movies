package integration

import java.time.Instant

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.review.*
import tools.IsolatedMongoDatabase

/** The answer history, as both stores keep it: empty at first, every answer read back whole, and in the order of the
 *  answers' own times (`at`), the order given breaking a tie -- whatever order they were appended in. The in-memory
 *  store kept append order until 2026-11-26, when it moved onto the keyed store beside Mongo's. */
class ReviewAnswerStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolated = IsolatedMongoDatabase.open(mongoTarget, "review-answer-contract")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val at     = Instant.parse("2026-10-06T08:00:00Z")
  private val member = ReviewMember("Kino A", "Film", Some("https://a/film"), Some(2001), Seq("Ada"))
  private def answered(verdict: ReviewVerdict, when: Instant = at, title: String = "Film") = ReviewAnswer(
    ReviewClusterId.of(Seq(member)), "pl", ReviewPage.Queue, verdict, Some(FilmRef.tmdb(7)),
    Some(FilmFacts(FilmRef.tmdb(1), Some("Shown"), Some(1999), Seq("Bo"))), title, Seq(member), "dev", when, Seq("warned"), Some("legacy-1"))

  private def mongo(): ReviewAnswerStore = {
    isolated.freshCollection(MongoReviewAnswerStore.Collection)
    new MongoReviewAnswerStore(isolated.database)
  }

  private val implementations: Seq[(String, () => ReviewAnswerStore)] =
    Seq("InMemory" -> (() => new InMemoryReviewAnswerStore), "MongoReviewAnswerStore" -> (() => mongo()))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "hold nothing until an answer is appended" in {
      fresh().all() shouldBe empty
    }

    it should "read an appended answer back whole" in {
      val store = fresh()
      store.append(answered(ReviewVerdict.Right))
      store.all() shouldBe Seq(answered(ReviewVerdict.Right))
    }

    it should "read answers of one instant in the order they were appended" in {
      val store = fresh()
      val titles = (1 to 12).map(i => s"T$i")
      titles.foreach(t => store.append(answered(ReviewVerdict.Right, title = t)))
      store.all().map(_.title) shouldBe titles
    }

    it should "read answers in the order of their own times, whatever order they were appended in" in {
      val store = fresh()
      store.append(answered(ReviewVerdict.Wrong, at))
      store.append(answered(ReviewVerdict.Right, at.minusSeconds(3600)))
      store.all().map(_.verdict) shouldBe Seq(ReviewVerdict.Right, ReviewVerdict.Wrong)
    }
  }
}
