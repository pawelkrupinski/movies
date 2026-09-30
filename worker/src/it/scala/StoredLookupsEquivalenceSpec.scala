package integration

import clients.tools.FakeHttpFetch
import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._

class StoredLookupsEquivalenceSpec extends AnyFlatSpec with Matchers {

  // Real UK captures, fetched at different times: "Thella Kagitham" (1776947) is popularity 0.4302
  // in its director's credits but 1.1742 in its title search — two buckets for one film.
  private val fetch    = new FakeHttpFetch("identity-popularity-drift", strict = true, foldYear = false)
  private val language = Country.UnitedKingdom.language
  private val director = CandidateQuery.Director("Ramesh Bandreddy")
  private val title    = CandidateQuery.Title("Thella Kagitham")

  private def asked(queries: CandidateQuery*) = {
    val memo = new IdentityShadow.Memo(new TmdbIdentityLookups(new clients.TmdbClient(fetch,
      apiKey = Some(settings.TmdbApiKey(IdentityShadow.StubTmdbKey)), language = language, retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil))
    queries.foreach(memo.candidates)
    memo
  }

  "The stored-lookups check" should "accept a film's one stored popularity when a recorded answer carried that bucket" in {
    val memo = asked(director, title)
    StoredLookupsEquivalence.check(fetch, () => 0L, () => Nil, language, memo, memo.asked).mismatches shouldBe empty
  }

  it should "still name a stored popularity no recorded answer carried" in {
    val memo = asked(director, title)
    val (queries, films) = memo.asked
    // The title search still fills the store, but recorded as a gap its bucket is one no recorded
    // answer carried — the director's answer must then differ on it.
    val mismatches = StoredLookupsEquivalence.check(fetch, () => 0L, () => Nil, language, memo,
      (queries.updated(title, Answer.Unknown), films)).mismatches
    mismatches.filter(_.startsWith(director.sortKey)) match {
      case Seq(line) => line should include ("vs stored [1456196:Thellakaagitham:2026:p-1, 1776947:Thella Kagitham:2026:p0]")
      case other     => fail(s"expected one mismatch on the director's answer, got $other")
    }
  }
}
