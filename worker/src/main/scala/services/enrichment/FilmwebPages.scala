package services.enrichment

import models.{MovieRecord, SourceData}
import services.enrichment.SiteRatings.Page

/** What a Filmweb read is to a row: the page a lookup or a details read becomes, the rating at the badge's precision,
 *  and what the row's own Filmweb slot says of the page it was written off. */
object FilmwebPages {

  /** A Filmweb read as a page: its rating at the precision the badge shows, and its content slot — the Filmweb
   *  counterpart of [[ImdbRatings.makeSlot]]: originalTitle, year, directors, Polish genres, Polish plot. A slot with no
   *  content at all is none, and drops the row's. */
  def page(info: FilmwebClient.FilmwebInfo): Page[Double] = {
    val slot = SourceData(
      originalTitle = info.originalTitle,
      releaseYear   = info.year,
      director      = info.directors,
      genres        = info.genres,
      synopsis      = info.plot
    )
    val hasContent = slot.originalTitle.isDefined || slot.releaseYear.isDefined ||
                     slot.director.nonEmpty || slot.genres.nonEmpty || slot.synopsis.isDefined
    Page(info.rating.map(rating), Option.when(hasContent)(slot))
  }

  /** A rating as stored: at the precision the badge shows (`%.1f`), so a sub-decimal vote drift the user can't see
   *  isn't a change — see RatingDisplay. */
  def rating(raw: Double): Double = RatingDisplay.oneDecimal(raw)

  /** Whether the row holds a Filmweb content slot. */
  def hasSlot(row: MovieRecord): Boolean = row.data.contains(models.Filmweb)

  /** The directors the row's Filmweb slot credits: the stored page's own credit, its slot written off that page. */
  def credit(row: MovieRecord): Set[String] = row.data.get(models.Filmweb).toSeq.flatMap(_.director).toSet
}
