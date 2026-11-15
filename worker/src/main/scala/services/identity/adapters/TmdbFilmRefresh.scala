package services.identity
package adapters

import clients.TmdbClient
import play.api.Logging
import services.enrichment.ImdbClient
import services.refresh.{RefreshableSource, RefreshableSources}
import services.tasks.BulkRefreshResult
import tools.{HttpFetch, HttpPath}

/**
 * An operator's "these TMDB films were just edited": each film's record fetched again into the model's TMDB store, then
 * every question of every family the films reach asked again, so the model re-resolves those families now — not a day
 * on, when `TmdbChangesSweep` reads the day's change list and `QuestionRenewals` finds the families' searches aged.
 *
 * A family is reached when one of its decisions takes, leans to or names as candidate one of the films, or one of its
 * listings bills, or one of its title searches asks (each folded by `fold`, the country's title key), a title the film's
 * fetched record carries: a Polish title just added on TMDB reaches the listings billing it, whose searches found nothing
 * before — a banner-decorated billing ("FOKUS: GRANICE - Bańki") through the search its peeled title asks.
 *
 * Every request goes through `fetch` (the normalizing lookup chain), so an answer that moved is filed and wakes the
 * model like any other (`TmdbStore.onChanged`), and one that did not writes nothing. `families` is the model's
 * ([[IdentityModelService.families]]): `None` while it is not taken up, when nothing is reached and the run says so.
 *
 * The `tmdb-films` [[RefreshableSource]]: its keys TMDB film ids.
 */
final class TmdbFilmRefresh(fetch: HttpFetch, tmdb: HttpFetch => TmdbClient, families: () => Option[Seq[TmdbFilmRefresh.Family]],
                            fold: String => String)
    extends RefreshableSource with Logging {
  import TmdbFilmRefresh._

  val name: String = RefreshableSources.TmdbFilms
  val keys: String = "TMDB film ids"

  def refresh(keys: Seq[String]): BulkRefreshResult = refreshFilms(keys.flatMap(_.toIntOption)).result

  def refreshFilms(films: Seq[Int]): Refreshed = HttpPath.within(HttpPath.Refresh) {
    val lookups = new TmdbIdentityLookups(tmdb(fetch), new ImdbClient(fetch), Nil, oneReadPerTitle = true)
    val records = films.distinct.map(film => film -> lookups.film(film))
    val titles  = records.flatMap {
      case (_, Answer.Known(Some(record))) => (record.title +: (record.originalTitle.toSeq ++ record.alternativeTitles)).map(fold)
      case _                               => Nil
    }.filter(_.nonEmpty).toSet
    val ids     = films.toSet
    val held    = families()
    val reached = held.getOrElse(Nil).filter(family => family.films.exists(ids) || family.titles.exists(title => titles(fold(title))) ||
      family.queries.exists { case CandidateQuery.Title(text) => titles(fold(text)); case _ => false })
    val queries = reached.flatMap(_.queries).distinct.sortBy(_.sortKey)
    val asked   = queries.map(lookups.candidates)
    val result  = Refreshed(films.distinct.size, records.count(_._2.isAnswered), held.isDefined, reached.size, queries.size,
      asked.count(_.isAnswered))
    logger.info(s"identity: TMDB films ${films.distinct.mkString(",")} refreshed — ${result.message}")
    result
  }
}

object TmdbFilmRefresh {
  /** One family as a refresh reads it: its questions, the films its decisions take, lean to or name as candidate, and
   *  its listings' titles, raw and as the model reads them. */
  final case class Family(queries: Set[CandidateQuery], films: Set[Int], titles: Set[String])

  object Family {
    /** Each family of `questions` (the model's `familyQuestions`), its listings' titles read off `listings`. */
    def of(questions: Seq[(Set[CandidateQuery], Seq[ResolverDecision])], listings: Seq[Listing]): Seq[Family] = {
      val titlesOf = listings.iterator.map(listing => listing.key -> Seq(listing.rawTitle, listing.title)).toMap
      questions.map { case (queries, decisions) =>
        Family(queries, decisions.flatMap(d => d.film.toSeq ++ d.leaning.map(_.film) ++ d.candidate.map(_.film)).toSet,
          decisions.flatMap(_.members).flatMap(key => titlesOf.getOrElse(key, Nil)).toSet)
      }
    }
  }

  /** What a refresh did: films asked and answered, whether the model was up, families reached, questions asked and
   *  answered. */
  final case class Refreshed(films: Int, records: Int, modelUp: Boolean, families: Int, questions: Int, answered: Int) {
    def message: String =
      s"$records of $films records fetched; " +
        (if (modelUp) s"$families families reached, $answered of $questions questions answered"
         else "no model taken up, so no family reached")
    def result: BulkRefreshResult = BulkRefreshResult.message(message)
  }
}
