package services.identity

import services.identity.agreement.AgreementStage.PosterQuestion
import services.identity.agreement.VoterFamily
import services.movies.ListingKey
import tools.HttpPath

/**
 * One question the identity system asks an outside source, whichever part asks it: the model's gaps (a candidate
 * query, a film's record, a venue's detail page) and the agreement stage's questions (a family's search, record or
 * programme, TMDB's find of an agreed IMDb id, a record read again, a poster, a catalogue page or id).
 *
 * It names WHAT is asked, not what the answer means: the model's answers still form candidates and the agreement's
 * still vote, veto and correct. One type is what lets asking, storing, remembering non-answers, waiting and measuring
 * be told the same way for both (docs/design/identity-questions.md). A set of them is the one currency for questions
 * asked, open and waited on: the agreement stage notes, keeps and hands its own to the queue as one (step C1).
 */
enum Question {
  /** A model gap: a title, person, IMDb or IMDb-any-language search ([[CandidateQuery]]). */
  case Candidate(query: CandidateQuery)
  /** A model gap: a film's TMDB record (`tmdb_films`), for a film an answer names. */
  case FilmRecord(tmdbId: Int)
  /** A model gap: the detail page of a listing at a venue with detail pages (`venue_pages`). */
  case VenuePage(listing: ListingKey)
  /** An agreement question to another film database: `question` as the agreement names it (`title|<text>`,
   *  `director|<name>`, `record|<id>`, `showing|<venue>`). */
  case Family(family: VoterFamily, question: String)
  /** An agreement question: TMDB's find of an agreed IMDb id. */
  case Find(imdbId: String)
  /** An agreement question: a TMDB record read again for its release day. */
  case Record(tmdbId: Int)
  /** An agreement question: a poster to hash. */
  case Poster(question: PosterQuestion)
  /** An agreement question: a catalogue page's links, or what a catalogue id maps to. */
  case Catalogue(question: CatalogueQuestion)

  def kind: QuestionKind = this match {
    case Candidate(CandidateQuery.Title(_))       => QuestionKind.Title
    case Candidate(CandidateQuery.Director(_))    => QuestionKind.Director
    case Candidate(CandidateQuery.Imdb(_))        => QuestionKind.Imdb
    case Candidate(CandidateQuery.ImdbTitled(_))  => QuestionKind.ImdbTitled
    case Candidate(CandidateQuery.TitleCredit(_)) => QuestionKind.TitleCredit
    case FilmRecord(_)                            => QuestionKind.FilmRecord
    case VenuePage(_)                             => QuestionKind.VenuePage
    // as the family store ages them (`FamilyAnswerStore.wanted`): by the stored id
    case Family(family, question) =>
      val id = s"${family.label}|$question"
      if (id.contains("|record|")) QuestionKind.FamilyRecord
      else if (id.contains("|showing|")) QuestionKind.FamilyShowing
      else QuestionKind.FamilySearch
    case Find(_)                                 => QuestionKind.Find
    case Record(_)                               => QuestionKind.Record
    case Poster(_)                               => QuestionKind.Poster
    case Catalogue(CatalogueQuestion.Page(_))    => QuestionKind.CataloguePage
    case Catalogue(CatalogueQuestion.Id(_))      => QuestionKind.CatalogueId
  }
}

object Question {

  /** The families' questions among `questions`, as the family store and its askers name them. */
  def families(questions: Iterable[Question]): Set[(VoterFamily, String)] =
    questions.iterator.collect { case Family(family, question) => family -> question }.toSet

  /** The model's gaps a change of answers names: its queries, its films' records, its listings' detail pages. */
  def ofAnswersChanged(changed: AnswersChanged): Set[Question] =
    changed.queries.map(Candidate(_)) ++ changed.films.map(FilmRecord(_)) ++ changed.details.map(VenuePage(_))
}

/** Which part of the identity system asks a kind of question. */
enum QuestionSystem {
  /** The model: its answers form candidates through a dependency chain. */
  case Model
  /** The agreement stage after it: its answers vote, veto or correct a take. */
  case Agreement
}

/**
 * A kind of [[Question]]: the unit a policy (store, freshness, non-answer memory, pacing) is set per. `askers` are the
 * [[HttpPath]]s its requests are counted under today, the asker first that asks it in the steady state.
 */
enum QuestionKind(val system: QuestionSystem, val askers: Seq[String]) {
  case Title       extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh))
  case Director    extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh))
  case Imdb        extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh))
  case ImdbTitled  extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh))
  case TitleCredit extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Refresh))
  case FilmRecord  extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.Fill, HttpPath.Live, HttpPath.Sweep))
  case VenuePage   extends QuestionKind(QuestionSystem.Model, Seq(HttpPath.VenuePage))

  case FamilySearch  extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case FamilyRecord  extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case FamilyShowing extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case Find          extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case Record        extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case Poster        extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case CataloguePage extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
  case CatalogueId   extends QuestionKind(QuestionSystem.Agreement, Seq(HttpPath.Agreement))
}
