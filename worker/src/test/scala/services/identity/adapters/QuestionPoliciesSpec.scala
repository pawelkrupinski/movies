package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.QuestionPolicies.{NonAnswer, Renewal, Sweep}
import services.identity.agreement.AgreementStage.{Open, PosterQuestion}
import services.identity.agreement.VoterFamily
import services.movies.ListingKey
import tools.HttpPath

import scala.concurrent.duration._

/** Every identity question, the model's and the agreement's, is one [[Question]] of a [[QuestionKind]] with a policy, and
 *  the table holds today's values: a change to one of them is a change to docs/design/identity-questions.md too. */
class QuestionPoliciesSpec extends AnyFlatSpec with Matchers {

  "QuestionPolicies" should "hold a policy for every kind of question" in {
    QuestionPolicies.table.keySet shouldBe QuestionKind.values.toSet
  }

  it should "hold today's stores, renewals, non-answer memory, sweeps and waits" in {
    val search = QuestionPolicies.Policy(
      "tmdb_queries",
      Seq(Renewal.After(1.day, "its cluster below 0.9 confidence"), Renewal.After(7.days, "settled")),
      NonAnswer.Marker(1.day, 5.minutes, 6.hours),
      Sweep.Unread(30.days),
      None
    )
    def family(age: FiniteDuration) =
      QuestionPolicies.Policy("identity_family_answers", Seq(Renewal.After(age, "any")), NonAnswer.EmptyFiled, Sweep.Fetched(400.days), Some(10.minutes))
    val expected = Map(
      QuestionKind.Title       -> search,
      QuestionKind.Director    -> search,
      QuestionKind.Imdb        -> search,
      QuestionKind.ImdbTitled  -> search,
      QuestionKind.TitleCredit -> search,
      QuestionKind.FilmRecord ->
        QuestionPolicies.Policy("tmdb_films", Seq(Renewal.ChangesFeed, Renewal.After(30.days, "read by the model")), NonAnswer.Marker(1.day, 5.minutes, 6.hours), Sweep.Unread(30.days), None),
      QuestionKind.VenuePage     -> QuestionPolicies.Policy("venue_pages", Seq(Renewal.After(6.hours, "any")), NonAnswer.GoneFiled, Sweep.Kept, None),
      QuestionKind.FamilySearch  -> family(90.days),
      QuestionKind.FamilyRecord  -> family(365.days),
      QuestionKind.FamilyShowing -> family(20.hours),
      QuestionKind.Find          -> QuestionPolicies.Policy("tmdb_queries", Nil, NonAnswer.EmptyFiled, Sweep.Unread(30.days), Some(10.minutes)),
      QuestionKind.Record        -> QuestionPolicies.Policy("tmdb_films", Nil, NonAnswer.EmptyFiled, Sweep.Unread(30.days), Some(10.minutes)),
      QuestionKind.Poster -> QuestionPolicies.Policy("identity_family_answers", Seq(Renewal.After(365.days, "hashed")), NonAnswer.GivenUp(8, 7.days),
        Sweep.Fetched(400.days), Some(10.minutes)),
      QuestionKind.CataloguePage -> family(90.days),
      QuestionKind.CatalogueId -> QuestionPolicies.Policy("identity_family_answers",
        Seq(Renewal.After(365.days, "mapped to a film"), Renewal.After(30.days, "mapped to none")), NonAnswer.EmptyFiled, Sweep.Fetched(400.days),
        Some(10.minutes))
    )
    QuestionKind.values.foreach(kind => (kind -> QuestionPolicies.of(kind)) shouldBe (kind -> expected(kind)))
  }

  "Question" should "name each kind the way its store ages it" in {
    Question.Candidate(CandidateQuery.Title("Lalka")).kind shouldBe QuestionKind.Title
    Question.Candidate(CandidateQuery.TitleCredit("Es (Ulrich Schamoni)")).kind shouldBe QuestionKind.TitleCredit
    Question.Family(VoterFamily.Imdb, "title|Lalka").kind shouldBe QuestionKind.FamilySearch
    Question.Family(VoterFamily.Wiki, "director|Wojciech Has").kind shouldBe QuestionKind.FamilySearch
    Question.Family(VoterFamily.Filmweb, "record|123").kind shouldBe QuestionKind.FamilyRecord
    Question.Family(VoterFamily.Filmweb, "showing|kino-muranow").kind shouldBe QuestionKind.FamilyShowing
    Question.Poster(PosterQuestion.Venue("https://example.org/p.jpg")).kind shouldBe QuestionKind.Poster
    Question.Catalogue(CatalogueQuestion.Page("https://example.org/film")).kind shouldBe QuestionKind.CataloguePage
    Question.Catalogue(CatalogueQuestion.Id(CatalogueId("letterboxd", "lalka"))).kind shouldBe QuestionKind.CatalogueId
  }

  it should "carry the agreement's open questions there and back unchanged" in {
    val open = Open(
      questions = Set(VoterFamily.Imdb -> "title|Lalka", VoterFamily.Filmweb -> "showing|kino-muranow"),
      finds = Set("tt0063208"),
      posters = Set(PosterQuestion.Film(7), PosterQuestion.Venue("https://example.org/p.jpg")),
      catalogue = Set(CatalogueQuestion.Page("https://example.org/film")),
      records = Set(7, 8)
    )
    val questions = Question.ofOpen(open)
    questions.size shouldBe 8
    questions.map(_.kind.system) shouldBe Set(QuestionSystem.Agreement)
    Question.toOpen(questions + Question.FilmRecord(7)) shouldBe open
  }

  it should "name the model's gaps a change of answers names, asked by the fill, live and the refreshes" in {
    val listing = ListingKey.Native("kino-muranow", "42", "Lalka")
    val gaps    = Question.ofAnswersChanged(AnswersChanged(Set(CandidateQuery.Title("Lalka")), Set(7), Set(listing)))
    gaps shouldBe Set(Question.Candidate(CandidateQuery.Title("Lalka")), Question.FilmRecord(7), Question.VenuePage(listing))
    gaps.map(_.kind.system) shouldBe Set(QuestionSystem.Model)
    QuestionKind.Title.askers shouldBe Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh)
    QuestionKind.values.flatMap(_.askers).toSet.subsetOf(HttpPath.all.toSet) shouldBe true
  }
}
