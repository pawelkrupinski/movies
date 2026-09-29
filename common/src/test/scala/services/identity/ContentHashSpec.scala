package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The slice digest a stored family keeps: equal content hashes equally however it was built, in
 *  every JVM, and any change to it — however deep — moves the hash. */
class ContentHashSpec extends AnyFlatSpec with Matchers {

  private def film(title: String, directors: Seq[String]) =
    Candidate(7, IdentityMeasures.Film(title, year = Some(1968), directors = Some(directors), popularity = Some(3.5)))
  private def slice(candidates: Seq[(Int, Candidate)], answers: Seq[(CandidateQuery, Option[Seq[Int]])]) =
    CorpusContext.Slice(Map.empty, Map.empty, Set("lalka"), Set.empty, Map("lalka" -> "has"), candidates.toMap, answers.toMap)

  private val lalka   = film("Lalka", Seq("Wojciech Has"))
  private val queries = Seq(CandidateQuery.Title("Lalka") -> Some(Seq(7, 9)), CandidateQuery.Director("Wojciech Has") -> None)

  "a slice's digest" should "be equal for equal content, whatever order its maps and sets were built in" in {
    slice(Seq(7 -> lalka, 9 -> film("Lalka", Nil)), queries).digest shouldBe
      slice(Seq(9 -> film("Lalka", Nil), 7 -> lalka), queries.reverse).digest
  }

  it should "move with any change, however deep in a record" in {
    val base = slice(Seq(7 -> lalka), queries).digest
    slice(Seq(7 -> film("Lalka", Seq("Wojciech J. Has"))), queries).digest should not be base
    slice(Seq(7 -> lalka), queries.map { case (q, a) => q -> a.map(_.reverse) }).digest should not be base
    slice(Seq(7 -> lalka), queries.take(1)).digest should not be base
  }

  it should "be the same in every JVM and run: it is persisted beside each stored family" in {
    // Pinned: a change here strands every stored family's digest (a full re-resolve on deploy).
    ContentHash.of(slice(Seq(7 -> lalka), queries)) shouldBe ContentHash.of(slice(Seq(7 -> lalka), queries))
    ContentHash.of(("lalka", Some(1968), Seq(1.5, 2.0), Set("a", "b"))) shouldBe PinnedTuple
  }

  private val PinnedTuple = 7427319714951912975L
}
