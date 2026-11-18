package controllers

import play.api.libs.json.{JsObject, Json}
import services.identity.staging.{StagedCluster, StagedFact, StagingCountry}

/** What `/debug/identity/staging/data` answers ([[IdentityStagingController]]): every country's
 *  staged clusters as the page reads them. */
object IdentityStagingJson {

  /** A cluster as the page reads it, with what its search matches besides the titles it shows: [[searchTitlesOf]]. */
  private def searchable(cluster: StagedCluster): JsObject =
    Json.toJsObject(cluster) + ("searchTitles" -> Json.toJson(searchTitlesOf(cluster)))

  /** The spellings the screen's title search matches besides a row's titles, as the city page's search does
   *  ([[SearchTitles]]): each listing's original title, and each film's (taken now, or before) original and alternative
   *  titles as TMDB states them — each also unaccented, an original in another script romanised. */
  def searchTitlesOf(cluster: StagedCluster): Seq[String] = {
    def fact(facts: Seq[StagedFact], name: String) = facts.find(_.name == name).map(_.value)
    val ofListings = cluster.listings.flatMap(l => SearchTitles.of(l.rawTitle, fact(l.facts, "original title"), Nil))
    val ofFilms    = (cluster.take +: cluster.before.toSeq).flatMap(take => take.title.toSeq.flatMap(title =>
      SearchTitles.of(title, fact(take.filmFacts, "original title"), fact(take.filmFacts, "alternative titles").toSeq.flatMap(_.split(", ")))))
    (ofListings ++ ofFilms).distinctBy(_.toLowerCase(java.util.Locale.ROOT))
  }

  /** Every country in its order, with a part or without (a country whose worker never published). */
  def countries(parts: Seq[StagingCountry]): Seq[JsObject] = {
    val byCode = parts.map(p => p.country -> p).toMap
    val known  = models.Country.all.map(_.code)
    (known ++ byCode.keys.filterNot(known.contains).toSeq.sorted).map { code =>
      val part = byCode.get(code)
      Json.obj(
        "code"    -> code,
        "name"    -> models.Country.all.find(_.code == code).fold(code)(_.displayName),
        "beat"    -> part.flatMap(_.beat),
        "changed" -> part.fold(Seq.empty[JsObject])(_.changed.map(searchable)),
        "keys"    -> part.fold(Seq.empty[String])(_.keys),
        "cursor"  -> part.flatMap(_.cursor).map(_.toEpochMilli))
    }
  }
}
