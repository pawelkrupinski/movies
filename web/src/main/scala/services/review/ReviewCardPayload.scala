package services.review

import play.api.libs.json.{JsObject, Json}

/** What a review card's answer buttons post back ([[ReviewCard.payload]]). */
object ReviewCardPayload {

  /** What `card`'s answer buttons post back on `page`: its Agent-judged take's own, else the cluster as it showed it. */
  def of(card: ReviewCard, page: ReviewPage): JsObject = card.item.fold(clusterPayload(card, page))(_.payload)

  private def clusterPayload(card: ReviewCard, page: ReviewPage): JsObject = {
    import card.{cluster, reviewMembers, shown}
    Json.obj(
      "clusterId" -> cluster.id, "country" -> cluster.country.code, "page" -> page.code, "title" -> cluster.title,
      // a listing the venues share is named once with its venues: a film at hundreds of venues of one aggregator page
      // otherwise repeated its every fact hundreds of times, most of a recently-matched page's weight
      "listings" -> reviewMembers.groupBy(m => (m.rawTitle, m.page, m.year, m.directors)).toSeq
        .sortBy { case (_, ms) => reviewMembers.indexOf(ms.head) }.map { case ((raw, page, year, directors), ms) =>
          Json.obj("rawTitle" -> raw, "page" -> page, "year" -> year, "directors" -> directors, "venues" -> ms.map(_.venue)) },
      "shown" -> shown.map(filmJson(card, _)),
      "films" -> (shown.toSeq ++ cluster.candidates.map(_.film)).distinct.map(filmJson(card, _)))
  }

  private def filmJson(card: ReviewCard, tmdb: Int): JsObject = {
    val f = card.filmFacts(tmdb)
    Json.obj("ref" -> f.ref.render, "title" -> f.title, "year" -> f.year, "directors" -> f.directors)
  }
}
