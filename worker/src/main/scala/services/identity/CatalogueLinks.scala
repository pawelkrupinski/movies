package services.identity

import java.net.URI
import java.util.Locale
import scala.util.Try
import scala.util.matching.Regex

/** A venue client's film pages that link their film in other catalogues: the hosts its pages live on and the catalogues
 *  it links ([[CatalogueLinks.Patterns]]'s sources) — declared by the client that scrapes them (`FlicksClient`,
 *  `KinotekaClient`), read on the agreement's queue for a cluster nothing else took ([[CatalogueAnswerStore]]). */
final case class CatalogueLinkPages(hosts: Set[String], sources: Set[String])

object CatalogueLinks {

  /** Each catalogue a page may link, by the link's shape: a Letterboxd film page, a Rotten Tomatoes movie page, an IMDb
   *  title. */
  val Patterns: Map[String, Regex] = Map(
    "letterboxd" -> """letterboxd\.com/film/([a-z0-9-]+)""".r,
    "rt"         -> """rottentomatoes\.com/m/([a-z0-9_-]+)""".r,
    "imdb"       -> """imdb\.com/title/(tt\d+)""".r)

  /** The catalogue ids `html` links, of `sources`: a catalogue it links ONE film of — a page linking two (a double bill,
   *  a "see also") names none by it. */
  def of(html: String, sources: Set[String]): Seq[CatalogueId] =
    sources.toSeq.sorted.flatMap(source => Patterns.get(source).toSeq.flatMap { pattern =>
      pattern.findAllMatchIn(html).map(_.group(1)).toSeq.distinct match {
        case Seq(only) => Seq(CatalogueId(source, only))
        case _         => Nil
      }
    })

  /** The declaration covering `page`'s host, if any of `pages` does. */
  def pagesOf(page: String, pages: Seq[CatalogueLinkPages]): Option[CatalogueLinkPages] =
    Try(Option(URI.create(page.trim).getHost)).toOption.flatten.map(_.toLowerCase(Locale.ROOT)).flatMap(host => pages.find(_.hosts(host)))
}
