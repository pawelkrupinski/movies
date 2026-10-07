package services.review

import play.api.libs.json._

import java.time.Instant

/**
 * An answer as the review page posts it: the card's payload ([[ReviewCard.payload]]), the verdict, and —
 * for "Other film" / "This film" — the pasted link or chosen candidate. The chosen film's facts are
 * checked against the venues' own (year, directors) here, so the warning is kept with the answer.
 */
object AnswerRequest {
  def parse(body: JsValue, who: String, at: Instant): Either[String, ReviewAnswer] = {
    def str(o: JsValue, name: String) = (o \ name).asOpt[String].map(_.trim).filter(_.nonEmpty)
    def filmOf(o: JsValue): Option[FilmFacts] = str(o, "ref").flatMap(FilmRef.parse).map(ref =>
      FilmFacts(ref, str(o, "title"), (o \ "year").asOpt[Int], (o \ "directors").asOpt[Seq[String]].getOrElse(Nil)))
    for {
      card      <- (body \ "card").toOption.toRight("no card")
      clusterId <- str(card, "clusterId").toRight("no clusterId")
      country   <- str(card, "country").toRight("no country")
      page      <- str(card, "page").flatMap(ReviewPage.byCode).toRight("unknown page")
      verdict   <- str(body, "verdict").flatMap(ReviewVerdict.byCode).toRight("unknown verdict")
      ref       <- (verdict, str(body, "ref")) match {
                     case (ReviewVerdict.Film, None)       => Left("Other film needs a link or ref")
                     case (ReviewVerdict.Film, Some(text)) => FilmRef.parse(text).map(Some(_)).toRight(s"not a film link: $text")
                     case _                                => Right(None)
                   }
    } yield {
      // each listing the payload names once, at every venue that lists it
      val members = (card \ "listings").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap(l =>
        (l \ "venues").asOpt[Seq[String]].getOrElse(Nil).map(venue => ReviewMember(venue, str(l, "rawTitle").getOrElse(""),
          str(l, "page"), (l \ "year").asOpt[Int], (l \ "directors").asOpt[Seq[String]].getOrElse(Nil))))
      val shown  = (card \ "shown").toOption.flatMap(filmOf)
      val films  = (card \ "films").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap(filmOf)
      val answer = ReviewAnswer(clusterId, country, page, verdict, ref, shown, str(card, "title").getOrElse(""), members, who, at)
      val chosen = answer.chosen.map(c => (shown.toSeq ++ films).find(_.ref == c).getOrElse(FilmFacts(c)))
      answer.copy(warnings = chosen.toSeq.flatMap(FactCheck.warnings(members, _)))
    }
  }
}
