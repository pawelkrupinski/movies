package controllers

import models.City

/** Builds the `sitemap.xml` body — a flat `<urlset>` of every crawlable page:
 *  the landing, each city listing, each city plan, and every film deep-link the
 *  city is currently showing. Pure (no I/O) so it unit-tests against a fixed
 *  corpus; [[MovieController.sitemap]] feeds it the warm read-model schedules.
 *
 *  Deliberately omits the `/{city}/filmy?...` browse-facet pages: they're thin
 *  filtered slices of the same corpus and would multiply the URL count without
 *  adding indexable content. The film deep-links carry the actual long-tail.
 */
object SitemapBuilder {

  /** @param origin  scheme + host, no trailing slash (`https://kinowo.fly.dev`)
   *  @param entries each city paired with the films it's currently showing
   *  @param lastmod optional W3C date stamp applied to every URL (read-model mtime) */
  def build(origin: String, entries: Seq[(City, Seq[FilmSchedule])], lastmod: Option[String] = None): String = {
    val sb = new StringBuilder
    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append('\n')
    sb.append("""<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""").append('\n')

    def url(loc: String, changefreq: String, priority: String): Unit = {
      sb.append("  <url><loc>").append(escape(origin + loc)).append("</loc>")
      lastmod.foreach(m => sb.append("<lastmod>").append(m).append("</lastmod>"))
      sb.append("<changefreq>").append(changefreq).append("</changefreq>")
      sb.append("<priority>").append(priority).append("</priority></url>\n")
    }

    url("/", "daily", "1.0")
    entries.foreach { case (city, films) =>
      url(s"/${city.slug}/", "daily", "0.8")
      url(s"/${city.slug}/plan", "daily", "0.7")
      // Distinct + sorted so the file is deterministic (stable across requests
      // and testable) regardless of the read model's iteration order.
      films.map(_.movie.title).distinct.sorted.foreach { title =>
        url(FilmHref(title, city), "daily", "0.6")
      }
    }

    sb.append("</urlset>\n")
    sb.toString
  }

  // `FilmHref` already %-encodes the title, so a `<loc>` won't contain a raw
  // space; but it can still hold characters XML requires escaped inside an
  // element (`&` chief among them, were a film URL ever to carry two params).
  // Escaping defensively keeps the document well-formed for every title.
  private def escape(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")
}
