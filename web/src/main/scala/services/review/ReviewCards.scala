package services.review

import play.api.libs.json.{JsObject, Json}
import services.movies.ListingKey

import java.time.Instant

/** One member listing of a card, with everything the venue itself said about it — its listing row and
 *  its own film page — kept apart from the catalogue ids its feed names it by. */
final case class MemberView(key: ListingKey, slot: Option[SlotFacts], page: Option[VenueFacts], feed: Option[ListingFeed]) {
  def venue: String = key.venue
  def pageUrl: Option[String] = key match { case ListingKey.Native(_, page, _) => Some(page); case _ => None }
  /** The year and directors the venue stated: its key's, else its listing row's, else its page's. */
  def member: ReviewMember = {
    val base = ReviewMember.of(key)
    val own  = (slot.map(_.facts).toSeq ++ page.toSeq)
    base.copy(year = base.year.orElse(own.flatMap(_.year).headOption),
      directors = if (base.directors.nonEmpty) base.directors else own.map(_.directors).find(_.nonEmpty).getOrElse(Nil))
  }
  def poster: Option[String]   = slot.flatMap(_.facts.poster).orElse(page.flatMap(_.poster))
  def synopsis: Option[String] = slot.flatMap(_.facts.synopsis).orElse(page.flatMap(_.synopsis))
}

/** A cluster as a review card renders it. `shown` is the film put forward (the match or best candidate);
 *  `updatedAt` the recently-matched page's stand-in for when it was decided. */
final case class ReviewCard(cluster: ReviewCluster, members: Seq[MemberView], films: Map[Int, FilmCard],
                            labels: Seq[LabelRow], answer: Option[ReviewAnswer], updatedAt: Option[Instant]) {
  def shown: Option[Int] = cluster.shown
  def filmFacts(tmdb: Int): FilmFacts = films.get(tmdb).fold(FilmFacts(FilmRef.tmdb(tmdb)))(_.facts)
  def reviewMembers: Seq[ReviewMember] = members.map(_.member)
  /** Where the venues' own facts contradict the film the card puts forward. */
  def warnings: Seq[String] = shown.toSeq.flatMap(film => FactCheck.warnings(reviewMembers, filmFacts(film)))

  /** What the page's answer buttons post back: the cluster as the card showed it. */
  def payload(page: ReviewPage): JsObject = Json.obj(
    "clusterId" -> cluster.id, "country" -> cluster.country.code, "page" -> page.code, "title" -> cluster.title,
    "members" -> reviewMembers.map(m => Json.obj("venue" -> m.venue, "rawTitle" -> m.rawTitle, "page" -> m.page,
      "year" -> m.year, "directors" -> m.directors)),
    "shown" -> shown.map(filmJson),
    "films" -> (shown.toSeq ++ cluster.candidates.map(_.film)).distinct.map(filmJson))

  private def filmJson(tmdb: Int): JsObject = {
    val f = filmFacts(tmdb)
    Json.obj("ref" -> f.ref.render, "title" -> f.title, "year" -> f.year, "directors" -> f.directors)
  }
}

/** Builds the cards of the clusters on screen — and reads only theirs. */
object ReviewCards {
  def build(source: ReviewSource, clusters: Seq[(ReviewCluster, Option[Instant])], labels: Seq[LabelRow],
            answers: ReviewAnswers.Index): Seq[ReviewCard] = {
    val keys   = clusters.flatMap(_._1.members)
    val slots  = source.slots(keys.map(ListingKey.serialised))
    val pages  = source.venuePages(keys.collect { case ListingKey.Native(_, page, _) => page })
    val feeds  = source.feeds(keys.map(k => k.venue -> k.rawTitle))
    val films  = source.films(clusters.flatMap { case (c, _) => c.film.toSeq ++ c.candidates.map(_.film) })
    clusters.map { case (cluster, at) =>
      val members = cluster.members.map(k => MemberView(k, slots.get(ListingKey.serialised(k)),
        k match { case ListingKey.Native(_, page, _) => pages.get(page); case _ => None }, feeds.get(k.venue -> k.rawTitle)))
      val code = cluster.country.code
      val overlay = labels.filter(l => l.country == code &&
        cluster.members.exists(m => m.rawTitle == l.rawTitle && (l.venue == "*" || l.venue == m.venue)))
      ReviewCard(cluster, members, films, overlay, answers.answerFor(cluster.id, cluster.reviewMembers), at)
    }
  }
}
