package tools

import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rule `QueryPlans.violations` holds every planned statement to — on hand-built plans, so each of
 *  its branches is shown failing without a Mongo. */
class QueryPlansSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {

  private def plan(stages: String*) =
    QueryPlans.Plan("films", new BsonDocument("find", new BsonString("films")).append("filter", new BsonDocument("key", new BsonString("x"))),
      stages, docsExamined = 1, keysExamined = 1)
  private val indexed = plan("FETCH", "IXSCAN")
  private val scanned = plan("COLLSCAN")
  private val sortedInMemory  = plan("SORT", "FETCH", "IXSCAN")

  "a plan served by an index" should "be no violation" in {
    QueryPlans.violations(Seq(indexed), Map.empty) shouldBe empty
  }

  "a collection scan, or an in-memory sort" should "each be a violation" in {
    QueryPlans.violations(Seq(scanned), Map.empty).loneElement should startWith("unindexed: films find filter{key}")
    QueryPlans.violations(Seq(sortedInMemory), Map.empty).loneElement should startWith("unindexed: films find filter{key}")
  }

  it should "be none when its shape is allowed" in {
    QueryPlans.violations(Seq(scanned), Map(scanned.shape -> "why")) shouldBe empty
  }

  "an allowance" should "be a violation once nothing it names scans" in {
    QueryPlans.violations(Seq(indexed), Map(indexed.shape -> "why")) shouldBe Seq(s"allowed, but no longer scans or sorts: ${indexed.shape}")
  }

  "nothing planned" should "be a violation, not a pass" in {
    QueryPlans.violations(Nil, Map.empty) shouldBe Seq("no command was planned")
  }
}
