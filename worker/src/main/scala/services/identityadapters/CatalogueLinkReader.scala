package services.identityadapters

import services.identity.*
import tools.{HttpFetch, HttpRead, HttpStatusException}

/** Reads a venue film page's catalogue links through the fetch its client scrapes with — `readers`, one per
 *  declaration; a page gone for good (404/410) links none, any other failure throws. */
final class CatalogueLinkReader(readers: Seq[(CatalogueLinkPages, HttpFetch)]) {
  val pages: Seq[CatalogueLinkPages] = readers.map(_._1)
  def links(page: String): Seq[CatalogueId] =
    CatalogueLinks.pagesOf(page, pages).flatMap(declared => readers.find(_._1 == declared)).fold(Seq.empty[CatalogueId]) { case (declared, fetch) =>
      try CatalogueLinks.of(HttpRead.page(fetch, declared.readAt(page)), declared.sources)
      catch { case e: HttpStatusException if HttpStatusException.isDurable(e.code) => Nil }
    }
}
