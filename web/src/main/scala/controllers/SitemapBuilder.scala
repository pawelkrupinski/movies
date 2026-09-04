package controllers

import models.City

/** Builds the `sitemap.xml` body — a flat `<urlset>` of every crawlable page:
 *  the landing, each city listing (plus each metro listing, for a city whose
 *  `/{slug}/` is a chooser), and every film deep-link the city
 *  is currently showing. Pure (no I/O) so it unit-tests against a fixed corpus;
 *  [[MovieController.sitemap]] feeds it the warm read-model schedules.
 *
 *  Deliberately omits the `/{city}/movies?...` browse-facet pages: they're thin
 *  filtered slices of the same corpus and would multiply the URL count without
 *  adding indexable content. The film deep-links carry the actual long-tail.
 *  (`robots.txt` disallows the facets outright, so a crawler that finds them
 *  through the genre pills doesn't spend budget there either.)
 *
 *  Carries no `<changefreq>` or `<priority>`: Google ignores both outright, and
 *  emitting them on every URL only made the file read as machine-filler. What
 *  IS kept is `<lastmod>` — but only on the URLs it's true for (see `build`).
 */
object SitemapBuilder {

  /** @param origin  this deployment's public ORIGIN, no path and no trailing
   *                 slash (`https://kinowo.net`, `https://showtimes.cc`). The
   *                 mount point is NOT part of it: each `<loc>` picks its own up
   *                 from [[CityPath]] / [[FilmHref]] / `country.mountPath`, the
   *                 same builders the pages use. Folding it into the origin
   *                 instead is how this file once advertised
   *                 `showtimes.cc/uk/uk/kent/movie/…` — the prefix added twice,
   *                 once here and once by `FilmHref`.
   *  @param country the country this deployment serves — the mount point for the
   *                 landing URL, which belongs to no city
   *  @param entries each city paired with the films it's currently showing
   *  @param lastmod the read model's mtime as a W3C date, stamped on the URLs
   *                 whose body IS the read model — the city listings and
   *                 film pages, all of which re-render on every projection. The
   *                 landing is left unstamped: it's a static city list that a
   *                 projection doesn't touch, and Google discards the lastmod
   *                 signal site-wide once it catches URLs claiming changes they
   *                 didn't make. */
  def build(origin: String, country: models.Country, entries: Seq[(City, Seq[FilmSchedule])],
            lastmod: Option[String] = None): String = {
    val sb = new StringBuilder
    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append('\n')
    sb.append("""<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""").append('\n')

    def url(loc: String, stamped: Boolean = true): Unit = {
      sb.append("  <url><loc>").append(escape(origin + loc)).append("</loc>")
      if (stamped) lastmod.foreach(m => sb.append("<lastmod>").append(m).append("</lastmod>"))
      sb.append("</url>\n")
    }

    url(country.mountPath, stamped = false)
    entries.foreach { case (city, films) =>
      url(CityPath(city) + "/")
      // Distinct + sorted so the file is deterministic (stable across requests
      // and testable) regardless of the read model's iteration order.
      // Keyed by the assigned slug, not the title: two films CAN share a
      // title, and de-duplicating on the title dropped one of them from the
      // index entirely.
      films.map(f => (f.slug, f.movie.title)).distinct.sortBy(_._1).foreach { case (slug, title) =>
        url(FilmHref.forSlug(slug, title, city))
      }
    }

    sb.append("</urlset>\n")
    sb.toString
  }

  /** The BRAND FRONT DOOR's `sitemap.xml`: a sitemap INDEX naming each country
   *  mounted under the apex, not a URL list of its own.
   *
   *  A crawler only ever reads `sitemap.xml` at a HOST's root, and the apex's
   *  root belongs to the country picker rather than to any one country — so the
   *  three Showtimes countries under `showtimes.cc/{code}/` have no other way to
   *  be discovered from the domain they actually live on. Poland is left out on
   *  purpose: it is a different host with its own root sitemap, and listing a
   *  cross-domain `<loc>` here would be ignored anyway — which is why the
   *  callers pass the countries mounted under a path prefix, and only those. */
  def index(countries: Seq[models.Country]): String = {
    val sb = new StringBuilder
    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append('\n')
    sb.append("""<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""").append('\n')
    countries.flatMap(_.webUrl).foreach { base =>
      sb.append("  <sitemap><loc>").append(escape(base + "/sitemap.xml")).append("</loc></sitemap>\n")
    }
    sb.append("</sitemapindex>\n")
    sb.toString
  }

  // Film URLs are slugs (`a-z0-9-`) and city URLs are slugs too, so in practice
  // nothing reaching here needs escaping. The one exception is a title that
  // folds to an empty slug, where `FilmHref` falls back to the `?title=` query
  // form — %-encoded, so still free of raw `&` or spaces. Escaping anyway keeps
  // the document well-formed no matter what a future URL shape carries.
  private def escape(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")
}
