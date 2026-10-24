package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.QuestionPolicies.{NonAnswer, Renewal, Sweep}
import services.identity.agreement.AgreementStage.PosterQuestion
import services.identity.agreement.VoterFamily
import services.movies.ListingKey
import services.tasks.TaskType
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

  it should "hand the agreement's open questions to the queue as the tasks a deployed worker reads: their types, keys and payloads" in {
    val open = Set[Question](Question.Family(VoterFamily.Imdb, "title|Lalka"), Question.Family(VoterFamily.Filmweb, "showing|kino-muranow"),
      Question.Find("tt0063208"), Question.Record(7), Question.Poster(PosterQuestion.Film(7)),
      Question.Poster(PosterQuestion.Venue("https://example.org/p.jpg")), Question.Catalogue(CatalogueQuestion.Page("https://example.org/film")),
      Question.Catalogue(CatalogueQuestion.Id(CatalogueId("letterboxd", "lalka"))), Question.FilmRecord(7))
    open.filter(_.kind.system == QuestionSystem.Agreement).size shouldBe 8
    val queue = new services.tasks.InMemoryTaskQueue
    val clock = tools.SpecClock.Pinned
    AgreementQuestions.enqueue(queue, open, clock)
    val tasks = Iterator.continually(queue.claim("w", 1.minute, clock.instant())).takeWhile(_.isDefined).flatten.toSeq
    def asking(id: String) = (TaskType.AskQuestion, s"agreement|$id", Map("questions" -> id))
    tasks.map(task => (task.taskType, task.dedupKey, task.payload)).toSet shouldBe Set(
      asking("imdb|title|Lalka"), asking("filmweb|showing|kino-muranow"), asking("find|tt0063208"), asking("tmdb|record|7"),
      asking("poster|film|7"), asking("poster|venue|https://example.org/p.jpg"), asking("catalogue-page|https://example.org/film"),
      asking("catalogue|letterboxd|lalka"))
    tasks.flatMap(AgreementQuestions.questionsOf).toSet shouldBe open.filter(_.kind.system == QuestionSystem.Agreement)
  }

  it should "name each agreement question by the id its store files or names it by, read back as the same question" in {
    val film = Question.Family(VoterFamily.RottenTomatoes, "record|m/klondike_2022")
    val ids = Seq[(Question, String)](
      Question.Family(VoterFamily.Imdb, "title|Lalka | Doll") -> FamilyAnswerStore.questionId(VoterFamily.Imdb, "title|Lalka | Doll"),
      film -> FamilyAnswerStore.recordId(VoterFamily.RottenTomatoes, "m/klondike_2022"),
      Question.Find("tt0063208") -> TmdbStore.findId("tt0063208"),
      Question.Record(7) -> services.identity.agreement.AgreementStage.recordReadId(7),
      Question.Poster(PosterQuestion.Film(7)) -> PosterAnswers.idOf(PosterQuestion.Film(7)),
      Question.Poster(PosterQuestion.Venue("https://example.org/p|1.jpg")) -> PosterAnswers.idOf(PosterQuestion.Venue("https://example.org/p|1.jpg")),
      Question.Catalogue(CatalogueQuestion.Id(CatalogueId("letterboxd", "lalka"))) -> CatalogueAnswerStore.idOf(CatalogueId("letterboxd", "lalka")),
      Question.Catalogue(CatalogueQuestion.Page("https://example.org/film")) -> CatalogueAnswerStore.pageId("https://example.org/film"))
    ids.map { case (question, _) => question.id } shouldBe ids.map { case (_, id) => Some(id) }
    ids.map { case (question, id) => Question.parse(id) shouldBe Some(question) }
    (Question.FilmRecord(7).id, Question.parse("nobody|title|Lalka"), Question.parse("tmdb|record|x")) shouldBe ((None, None, None))
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
