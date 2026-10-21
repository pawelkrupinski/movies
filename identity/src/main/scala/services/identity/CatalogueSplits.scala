package services.identity

import services.identity.SuspectTakes.Take

/**
 * CATALOGUE SPLITS: one catalogue id whose listings were taken as two films. A catalogue names one film by one id
 * ([[CatalogueId]]), so one side is wrong — or the feed filed a namesake under the id and copied its facts with it,
 * which then read as the listing's own evidence. Measured on the prod mirrors 2026-10-21: one Webedia id taken as
 * Kästner's 1931 "Emil und die Detektive" and as the 2026 film (DE ×134), as two "Sentido y sensibilidad" (ES ×83, 1995
 * and 2026) and two "La invitación" (×14). A finding about the recording's data for a person to triage (the Identity
 * findings report), never a veto: which side is wrong is the judge's call.
 *
 * Box offices' own ids are scoped by their venue already ([[CatalogueSources.BoxOfficeOwn]]), so their per-venue numbering
 * meeting by coincidence is no split.
 */
object CatalogueSplits {

  /** The takes of `film` under `id` — `listings` of them — beside `others`, every other film its listings were taken as
   *  with how many, most first. */
  final case class Split(id: CatalogueId, film: String, title: String, takes: Seq[Take], others: Seq[(String, Int)]) {
    def venues: Seq[String]  = takes.map(_.listing.venue).distinct.sorted
    def rawTitles: Seq[String] = takes.map(_.listing.rawTitle).distinct.sorted
    def why: String =
      s"${id.source} ${id.id} also names ${others.map((f, n) => s"$f (${listings(n)})").mkString(", ")}; this film ${listings(takes.size)}"
  }

  private def listings(n: Int) = if (n == 1) "1 listing" else s"$n listings"

  /** Every film a catalogue id's listings were taken as, where they were taken as at least two: one [[Split]] per (id,
   *  film), each listing counted once. */
  def of(takes: Seq[Take]): Seq[Split] =
    takes.distinctBy(_.listing.key).flatMap(t => t.listing.catalogueIds.map(_ -> t)).groupMap(_._1)(_._2).toSeq
      .sortBy(_._1.key).flatMap { case (id, carrying) =>
        val byFilm = carrying.groupBy(_.film)
        if (byFilm.sizeIs < 2) Nil
        else byFilm.toSeq.sortBy { case (film, ts) => (-ts.size, film) }.map { case (film, ts) =>
          Split(id, film, ts.head.record.title + ts.head.record.year.fold("")(y => s" ($y)"), ts,
            byFilm.removed(film).toSeq.map((f, o) => f -> o.size).sortBy { case (f, n) => (-n, f) })
        }
      }
}
