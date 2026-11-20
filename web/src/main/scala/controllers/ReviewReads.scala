package controllers

import java.time.Instant

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.{Failure, Success, Try}

import models.Country
import services.identity.measures.ResolverDecision
import services.review.{DigestItem, LabelRow, ReviewAnswers, ReviewCard, ReviewCards, ReviewCluster, ReviewDigest, ReviewSource}

/**
 * What the dev-only review pages read, country by country: each country's [[ReviewSource]] over the LOCAL read-mirror
 * (`sources`, built by the wiring from `MONGODB_MOVIES_MIRROR_URI` alone; none without a mirror), its clusters and the
 * cards of the clusters on screen; one listing's or cluster's own reads are [[ReviewListingReads]]'. The countries are read side by side, so a page over every country waits on its
 * slowest, not on their sum; a country whose read failed is reported, never shown as one with no clusters.
 */
final class ReviewReads(sources: Map[Country, ReviewSource]) {

  /** Whether a mirror holds `country`. */
  def holds(country: Country): Boolean = sources.contains(country)

  /** The countries `code` selects that a mirror holds: every one of them for none or `all`. */
  def countriesOf(code: Option[String]): Seq[Country] =
    code.filterNot(_ == "all").flatMap(Country.byCode).fold(Country.all.filter(holds))(c => Seq(c).filter(holds))

  def sourceOf(country: Country): ReviewSource = sources.getOrElse(country, ReviewSource.empty)

  /** `read` of each country, side by side: a page over every country waits on its slowest, not on their sum. Each
   *  read bounds its own Mongo calls. */
  def perCountry[A](countries: Seq[Country])(read: Country => A): Seq[(Country, Try[A])] = {
    val reads = countries.map(c => c -> Future(blocking(Try(read(c))))(using ExecutionContext.global))
    reads.map { case (c, f) => c -> Await.result(f, Duration.Inf) }
  }

  /** Each country's `Seq` read, the failed ones reported rather than shown as none. */
  def reported[A](read: Seq[(Country, Try[Seq[A]])]): (Seq[A], Seq[String]) =
    (read.collect { case (_, Success(cs)) => cs }.flatten,
      read.collect { case (c, Failure(e)) => s"${c.code}: could not read identity_model_families (${e.getMessage})" })

  // The clusters of each decisions read, kept as long as the read is: a source that keeps its reads answers the same
  // one until it reads again, so its clusters, their ids and member keys are worked out once. Weak keys compare by
  // identity; an empty read (`Nil`, shared) is no clusters whichever country it came from.
  private val clustersKept = tools.BoundedCache.ofSize(16).weakKeys().build[Seq[ResolverDecision], Seq[ReviewCluster]]()

  /** Each selected country's clusters, a failed read reported rather than shown as no clusters. */
  def clustersOf(countries: Seq[Country], unmatchedOnly: Boolean): (Seq[ReviewCluster], Seq[String]) =
    chosenOf(countries, unmatchedOnly)(_ => identity)

  /** What `choose` picks of each selected country's clusters, each country's on its own thread; a failed read reported
   *  rather than shown as no clusters. */
  def chosenOf[A](countries: Seq[Country], unmatchedOnly: Boolean)(choose: Country => Seq[ReviewCluster] => Seq[A]): (Seq[A], Seq[String]) =
    reported(perCountry(countries)(c => choose(c)(clustersKept.get(sources(c).decisions(unmatchedOnly), _.map(ReviewCluster.of(c, _))))))

  /** Each Agent-judged item's cluster, in the items' order, each country's read on its own thread; a failed read
   *  reported rather than shown as no clusters. */
  def clustersOf(items: Seq[DigestItem]): (Seq[(ReviewCluster, DigestItem)], Seq[String]) = {
    val byCountry = items.groupBy(_.country)
    val (clusters, errors) = reported(perCountry(byCountry.keys.toSeq.flatMap(Country.byCode)) { c =>
      val decisions = sourceOf(c).decisions(unmatchedOnly = false)
      byCountry(c.code).map(i => ReviewDigest.clusterOf(c, i, decisions) -> i)
    })
    val order = items.map(_.id).zipWithIndex.toMap
    (clusters.sortBy { case (_, i) => order(i.id) }, errors)
  }

  /** The cluster `id` of `country`'s decisions: none when it is not there or they could not be read. */
  def cluster(country: Country, id: String): Option[ReviewCluster] =
    Try(sourceOf(country).decisions(unmatchedOnly = false)).toOption.flatMap(_.iterator.map(ReviewCluster.of(country, _)).find(_.id == id))

  /** The cards of `shown`, each country's built from its own source on its own thread. */
  def cards(shown: Seq[(ReviewCluster, Option[Instant])], labels: Seq[LabelRow], index: ReviewAnswers.Index): Seq[ReviewCard] = {
    val byCountry = shown.groupBy(_._1.country)
    perCountry(byCountry.keys.toSeq)(c => ReviewCards.build(sourceOf(c), byCountry(c), labels, index)).flatMap(_._2.get)
  }
}
