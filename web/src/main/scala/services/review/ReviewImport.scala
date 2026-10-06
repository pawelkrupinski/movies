package services.review

import play.api.libs.json._

import java.time.Instant

/**
 * The answers given on the hand-built review pages (the published artifacts), as [[ReviewAnswer]]s.
 * Each item carries the page it was given on (`review` — the queue —, `matchable`, `recent`), the
 * verdict and pasted ref, the cluster's members, and the film the card showed: the labelled film on
 * `matchable`, the resolver's match on `recent`, the best candidate on the queue.
 */
object ReviewImport {

  def parse(json: String): Seq[ReviewAnswer] = Json.parse(json).as[Seq[JsObject]].flatMap(answerOf)

  private def str(o: JsValue, name: String): Option[String] = (o \ name).asOpt[String].map(_.trim).filter(_.nonEmpty)
  private def int(o: JsValue, name: String): Option[Int] =
    (o \ name).asOpt[Int].orElse(str(o, name).flatMap(_.toIntOption))
  private def strings(o: JsValue, name: String): Seq[String] = (o \ name).asOpt[Seq[String]].getOrElse(Nil)

  /** A film object of the export (`labelledFilm`, `matchedFilm`, a `modelCandidates` entry). */
  private def filmOf(o: JsValue): Option[FilmFacts] = {
    val ref = str(o, "ref").flatMap(FilmRef.parse).orElse(int(o, "tmdb").map(FilmRef.tmdb)).orElse(str(o, "imdb").flatMap(FilmRef.parse))
    ref.map(r => FilmFacts(r, str(o, "title").filterNot(_ == r.render), int(o, "year"), strings(o, "directors")))
  }

  private def answerOf(o: JsObject): Option[ReviewAnswer] = for {
    page    <- str(o, "page").flatMap(ReviewPage.byCode)
    verdict <- str(o, "verdict").flatMap(ReviewVerdict.byCode)
  } yield {
    val title   = str(o, "title").getOrElse("")
    val members = (o \ "members").asOpt[Seq[JsObject]].getOrElse(Nil).map { m =>
      val facts = (m \ "venueFacts").toOption.getOrElse(JsObject.empty)
      ReviewMember(str(m, "venue").getOrElse(""), title, str(m, "page"), int(facts, "year"), strings(facts, "directors"))
    }
    val candidates = (o \ "modelCandidates").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap(filmOf)
    val shown = page match {
      case ReviewPage.Matchable => (o \ "labelledFilm").toOption.flatMap(filmOf)
      case ReviewPage.Recent    => (o \ "matchedFilm").toOption.flatMap(filmOf)
      case ReviewPage.Queue     => candidates.headOption
    }
    val ref   = str(o, "ref").flatMap(FilmRef.parse).orElse(int(o, "tmdb").map(FilmRef.tmdb))
    val draft = ReviewAnswer(ReviewClusterId.of(members), str(o, "country").getOrElse(""), page, verdict, ref, shown, title, members,
      who = "import", at = str(o, "at").map(Instant.parse).getOrElse(Instant.EPOCH), legacyId = str(o, "itemId"))
    val chosenFacts = draft.chosen.flatMap(chosen => (shown.toSeq ++ candidates).find(_.ref == chosen))
    draft.copy(warnings = chosenFacts.toSeq.flatMap(FactCheck.warnings(members, _)))
  }
}
