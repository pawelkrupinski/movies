package controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.identity.staging.{IdentityStaging, StagedCluster, StagedFact, StagingCountry}

import java.time.Instant

/**
 * `/debug/identity/staging` — the staging screen (docs/design/identity-questions.md, "The staging screen"): every
 * country's identity clusters whose takes still wait on an answer, and those decided, moved or settled lately, on one
 * page. Each country's worker publishes its own to its database ([[IdentityStaging]]); a local dev server reads every
 * country's (`stores`, one per country: its prod mirror, as the rest of /debug reads).
 *
 * The page renders a shell and polls `/debug/identity/staging/data` every few seconds, passing back each country's
 * cursor: a poll reads each collection's small index and only the clusters written since, so it costs the clusters
 * that moved, not the whole screen. Dev-only: 404 in prod, like the rest of /debug ([[DevMode]]).
 */
class IdentityStagingController(cc: ControllerComponents, environment: play.api.Mode, stores: Seq[IdentityStaging],
                                country: models.Country, clock: java.time.Clock,
                                pollMillis: Int = IdentityStagingController.PollMillis) extends AbstractController(cc) {
  import IdentityStagingController._

  def index: Action[AnyContent] = Action {
    DevMode.gate(environment)(Ok(views.html.admin.identityStaging(country, pollMillis)))
  }

  /** `since`: each country's cursor from the last answer, `pl:<millis>,uk:<millis>`; none, every cluster. */
  def data(since: Option[String]): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      val wanted = cursors(since)
      Ok(Json.obj("now" -> clock.millis(), "countries" -> answer(stores.flatMap(_.read(wanted)))))
    }
  }
}

object IdentityStagingController {
  /** How often the page polls. */
  val PollMillis = 5000

  /** The cursors a `since` parameter names; a malformed part names none. */
  def cursors(since: Option[String]): Map[String, Instant] =
    since.toSeq.flatMap(_.split(',')).flatMap(_.split(':') match {
      case Array(code, millis) => millis.toLongOption.map(code -> Instant.ofEpochMilli(_))
      case _                   => None
    }).toMap

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
  def answer(parts: Seq[StagingCountry]): Seq[JsObject] = {
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
