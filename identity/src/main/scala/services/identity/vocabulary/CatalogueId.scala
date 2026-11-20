package services.identity.vocabulary

import models.{Cinema, CinemaMovie}

/** A film's id in a cinema chain's own catalogue (`CinemaMovie.externalIds`: Gatsby's "boxoffice",
 *  Flicks', Cinema City's "cc", Multikino's "mk"…): the chain lists one film under one id at every
 *  venue, a bare listing beside its credited siblings. Measured on recording 36633286921 across the
 *  five corpora: no chain reuses an id for another film, and its ids are global to it. A box office's own
 *  event numbering is not ([[CatalogueSources.BoxOfficeOwn]]): its id is scoped by the venue that published it. */
final case class CatalogueId(source: String, id: String) {
  /** Its family block key and must-link identity. */
  def key: String = s"c:$source:$id"
}

object CatalogueId {
  given Ordering[CatalogueId] = Ordering.by(c => (c.source, c.id))

  /** `cm`'s catalogue ids as `cinema` publishes them, blank ones dropped: a box office's own event id
   *  ([[CatalogueSources.BoxOfficeOwn]]) under its venue's name, so another venue's system numbering another film
   *  the same never names one film with it. */
  def of(cinema: Cinema, cm: CinemaMovie): Seq[CatalogueId] =
    cm.externalIds.toSeq.collect { case (source, id) if source.trim.nonEmpty && id.trim.nonEmpty =>
      val own = id.trim
      CatalogueId(source.trim, if (CatalogueSources.BoxOfficeOwn(source.trim)) s"${cinema.displayName}/$own" else own)
    }.sorted
}
