package services.identity.adapters

import services.freshness.{Freshness, FreshnessKind}
import services.identity.QuestionKind
import services.identity.agreement.AgreementStage
import services.venuepages.MongoVenuePageStore

import scala.concurrent.duration._

/**
 * What each kind of identity question is held to today, in one table: where its answer is stored, when it is asked
 * again, what is remembered when the source gives no answer, how long an unread answer is kept, and how long a take
 * waits on it. Every value is the constant the code that does it reads: this table describes, it decides nothing
 * (docs/design/identity-questions.md holds it as prose, [[QuestionPoliciesSpec]] keeps the two in step).
 *
 * The first step of one asker for every question (the doc's plan): what differs between the rows is what a shared
 * asker, non-answer memory and settle rule will have to keep or deliberately change.
 */
object QuestionPolicies {

  /** When a stored answer is asked again: past an age (`when` says which answers), or when a feed names it. */
  enum Renewal {
    case After(age: FiniteDuration, when: String)
    /** TMDB's daily change lists (`TmdbChangesSweep`), and no age. */
    case ChangesFeed
  }

  /** What a source giving no answer leaves behind. */
  enum NonAnswer {
    /** An `unanswered|` marker ([[TmdbGapMemory]], `identity_question_markers`), written by the fill and the model's live
     *  asks alike: an empty answer held back `empty`, a failure `firstFailure` doubling to `maxFailure`; markers go
     *  [[TmdbStoreSweep.MarkerGrace]] after their wait. */
    case Marker(empty: FiniteDuration, firstFailure: FiniteDuration, maxFailure: FiniteDuration)
    /** An empty answer is filed as one ("no film"); a failure files nothing and is asked again on the task queue's backoff. */
    case EmptyFiled
    /** A gone page (404/410) is filed; a failed read is marked attempted (`EnrichDetailsTasks.pageAttempted`) and read
     *  again past the detail refresh's age, one the store did not take on the task queue's backoff. */
    case GoneFiled
    /** Given up on after `attempts` failed fetches, filed as unread and tried again after `retry`. */
    case GivenUp(attempts: Int, retry: FiniteDuration)
  }

  /** When the store sweep ([[TmdbStoreSweep]]) deletes an answer. */
  enum Sweep {
    /** Unread by the model taken up, and unfetched, this long. */
    case Unread(after: FiniteDuration)
    /** Unfetched this long, read or not. */
    case Fetched(after: FiniteDuration)
    /** Never. */
    case Kept
  }

  final case class Policy(
    store: String,
    renewal: Seq[Renewal],
    nonAnswer: NonAnswer,
    swept: Sweep,
    /** How long a take waits on an open question of this kind before deciding without it; `None`: no wait of its own
     *  (the model's gaps wait on the lookup fill's rounds). */
    waitedOn: Option[FiniteDuration]
  )

  private val tmdbMemory = NonAnswer.Marker(TmdbGapMemory.RetryAfter, TmdbGapMemory.FirstFailureRetry, TmdbGapMemory.MaxFailureRetry)

  private val search = Policy(
    TmdbKind.Query.collection,
    Seq(
      Renewal.After(TmdbRefreshes.UnsettledAge, s"its cluster below ${TmdbRefreshes.Settled} confidence"),
      Renewal.After(TmdbRefreshes.SettledAge, "settled")
    ),
    tmdbMemory,
    Sweep.Unread(TmdbStoreSweep.KeepUnread),
    None
  )

  private def family(age: FiniteDuration) =
    Policy(TmdbKind.Family.collection, Seq(Renewal.After(age, "any")), NonAnswer.EmptyFiled, Sweep.Fetched(TmdbStoreSweep.KeepFamily), Some(AgreementStage.PartialAfter))

  private def agreement(store: String, renewal: Renewal*) =
    Policy(store, renewal, NonAnswer.EmptyFiled, Sweep.Fetched(TmdbStoreSweep.KeepFamily), Some(AgreementStage.PartialAfter))

  val table: Map[QuestionKind, Policy] = Map(
    QuestionKind.Title       -> search,
    QuestionKind.Director    -> search,
    QuestionKind.Imdb        -> search,
    QuestionKind.ImdbTitled  -> search,
    QuestionKind.TitleCredit -> search,
    QuestionKind.FilmRecord  -> Policy(TmdbKind.Film.collection, Seq(Renewal.ChangesFeed), tmdbMemory, Sweep.Unread(TmdbStoreSweep.KeepUnread), None),
    QuestionKind.VenuePage -> Policy(
      MongoVenuePageStore.Collection,
      Seq(Renewal.After(Freshness.ttlFor(FreshnessKind.DetailEnrich).get, "any")),
      NonAnswer.GoneFiled,
      Sweep.Kept,
      None
    ),
    QuestionKind.FamilySearch  -> family(FamilyAnswerStore.SearchAge),
    QuestionKind.FamilyRecord  -> family(FamilyAnswerStore.RecordAge),
    QuestionKind.FamilyShowing -> family(FamilyAnswerStore.ShowingAge),
    // TMDB's answers, filed in the TMDB store as the model's are, through the TMDB client: a find naming no film is filed
    // as one (`TmdbStore.foundFirst`), a record TMDB does not know as empty; a failure is asked again on the task
    // queue's backoff. A record read again is asked once in the stage's life, when a take waits on its release day.
    QuestionKind.Find   -> agreement(TmdbKind.Query.collection).copy(swept = Sweep.Unread(TmdbStoreSweep.KeepUnread)),
    QuestionKind.Record -> agreement(TmdbKind.Film.collection).copy(swept = Sweep.Unread(TmdbStoreSweep.KeepUnread)),
    QuestionKind.Poster -> agreement(TmdbKind.Family.collection, Renewal.After(PosterAnswerStore.Age, "hashed"))
      .copy(nonAnswer = NonAnswer.GivenUp(PosterAnswerStore.GiveUpAttempts, PosterAnswerStore.UnreadAge)),
    QuestionKind.CataloguePage -> agreement(TmdbKind.Family.collection, Renewal.After(CatalogueAnswerStore.PageAge, "any")),
    QuestionKind.CatalogueId -> agreement(
      TmdbKind.Family.collection,
      Renewal.After(CatalogueAnswerStore.MappedAge, "mapped to a film"),
      Renewal.After(CatalogueAnswerStore.UnmappedAge, "mapped to none")
    )
  )

  def of(kind: QuestionKind): Policy = table(kind)
}
