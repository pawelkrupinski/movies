package controllers

import models.Country
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.review.{ReviewAnswers, ReviewSource, TmdbPosterLookup}

import java.nio.file.Path
import java.time.Clock

/** The review pages' controllers wired as `ReviewWiring` wires them, over one set of sources, answers and labels file,
 *  each route answered by the controller the routes file names. */
final class ReviewPages(cc: ControllerComponents, environment: Mode, sources: Map[Country, ReviewSource], answers: ReviewAnswers,
                        labelsPath: Path, clock: Clock, notices: Seq[String] = Nil, tmdbPosters: Option[TmdbPosterLookup] = None) {
  private val reads  = new ReviewReads(sources)
  private val listingReads = new ReviewListingReads(reads)
  private val labels = new ReviewLabels(labelsPath)
  private val cards  = new ReviewCardsPage(answers, reads, notices)
  val queues      = new ReviewQueueController(cc, environment, new ReviewQueues(reads, labels, clock), cards)
  val agentJudgedPages = { val judged = new AgentJudgedCards(tmdbPosters); new AgentJudgedController(cc, environment, new AgentJudgedTakes(answers, labels, judged), reads, judged, cards) }
  val listings    = new ReviewListingController(cc, environment, listingReads, labels, notices)
  val posters     = new TmdbPosterController(cc, environment, tmdbPosters)

  def queue(country: Option[String], limit: Int, answered: Boolean) = queues.queue(country, limit, answered)
  def matchable(country: Option[String], min: Double, basis: Option[String], limit: Int, answered: Boolean) =
    queues.matchable(country, min, basis, limit, answered)
  def recent(country: Option[String], hours: Int, limit: Int, answered: Boolean) = queues.recent(country, hours, limit, answered)
  def agentJudged(country: Option[String], kind: Option[String], limit: Int, answered: Boolean) =
    agentJudgedPages.agentJudged(country, kind, limit, answered)
  def digestMoved(country: Option[String]) = agentJudgedPages.digestMoved(country)
  def why(country: String, cluster: String) = listings.why(country, cluster)
  def listing(country: String, venue: Option[String], title: String) = listings.listing(country, venue, title)
  def tmdbPoster(tmdbId: Int, country: Option[String]) = posters.tmdbPoster(tmdbId, country)
}
