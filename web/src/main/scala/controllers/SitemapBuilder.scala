package controllers

import models.CityPath

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
   *  @param lastmod per-CITY mtime as a W3C date, stamped on every URL whose
   *                 body is that city's slice of the read model — the city's
   *                 own listing and every film page under it, all of which
   *                 re-render on every projection touching that city. A
   *                 function rather than one shared value: stamping every city
   *                 with the model-wide `readModel.lastModified` claimed a
   *                 Warsaw showtime edit as a change to London's URLs too, and
   *                 Google discards the lastmod signal site-wide once it
   *                 catches URLs claiming changes they didn't make — the exact
   *                 failure mode `readModel.lastModifiedFor(city)` already
   *                 exists to avoid for conditional GETs (see
   *                 `WebReadModel.lastModifiedFor`). The landing is always left
   *                 unstamped: it's a static city list no per-city projection
   *                 touches.
   *  @param includeLanding whether to emit the country's landing URL. `false`
   *                 for a per-city sub-sitemap (see [[MovieController.citySitemap]]):
   *                 the landing already has its own address once the flat file
   *                 is partitioned, and repeating it in every sub-file would
   *                 advertise the same `<loc>` from several sitemaps at once. */
  def build(origin: String, country: models.Country, entries: Seq[(City, Seq[FilmSchedule])],
            lastmod: City => Option[String] = _ => None, includeLanding: Boolean = true): String = {
    val sb = new StringBuilder
    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append('\n')
    sb.append("""<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""").append('\n')

    def url(loc: String, stamp: Option[String]): Unit = {
      sb.append("  <url><loc>").append(escape(origin + loc)).append("</loc>")
      stamp.foreach(m => sb.append("<lastmod>").append(m).append("</lastmod>"))
      sb.append("</url>\n")
    }

    if (includeLanding) url(country.mountPath, None)
    entries.foreach { case (city, films) =>
      val stamp = lastmod(city)
      url(CityPath(city) + "/", stamp)
      // Distinct + sorted so the file is deterministic (stable across requests
      // and testable) regardless of the read model's iteration order.
      // Keyed by the assigned slug, not the title: two films CAN share a
      // title, and de-duplicating on the title dropped one of them from the
      // index entirely.
      films.map(f => (f.slug, f.movie.title)).distinct.sortBy(_._1).foreach { case (slug, title) =>
        url(FilmHref.forSlug(slug, title, city), stamp)
      }
    }

    sb.append("</urlset>\n")
    sb.toString
  }

  /** How many `<url>` elements [[build]] would emit for `entries`, landing URL
   *  included — [[MovieController.sitemap]]'s threshold check for whether a
   *  country's flat file needs partitioning into [[cityIndex]] instead. Kept in
   *  lock-step with `build`'s own de-duplication so the count it decides on is
   *  the count that would actually be written. */
  def urlCount(entries: Seq[(City, Seq[FilmSchedule])]): Int =
    1 + entries.map { case (_, films) => 1 + films.map(f => (f.slug, f.movie.title)).distinct.size }.sum

  /** A country's URL budget above which its flat `sitemap.xml` is partitioned
   *  into a per-city [[cityIndex]] instead (see [[MovieController.sitemap]]).
   *  Comfortably under the sitemap protocol's hard cap of 50,000 URLs / 50MB
   *  per file — chosen so a country crosses it well before that limit becomes
   *  a real risk, not right at the edge of it. Not a per-country constant: the
   *  check is against the corpus's actual count, so it applies to whichever
   *  country's corpus grows past it next, not just the one large enough today. */
  val CityPartitionThreshold: Int = 10000

  /** A per-country sitemap INDEX, one `<sitemap>` per city plus one for the
   *  landing page — used by [[MovieController.sitemap]] instead of [[build]]'s
   *  flat file once [[urlCount]] passes [[CityPartitionThreshold]]. Each city's
   *  `<loc>` is `/{city}/sitemap.xml` ([[MovieController.citySitemap]]), a full
   *  flat file scoped to that one city — small enough it never needs
   *  partitioning again on its own. The landing page gets its own address
   *  ([[MovieController.sitemapRoot]]) rather than riding along in one city's
   *  file, where it would look like that city's page to a crawler. */
  def cityIndex(origin: String, country: models.Country): String = {
    val sb = new StringBuilder
    sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append('\n')
    sb.append("""<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">""").append('\n')
    sb.append("  <sitemap><loc>").append(escape(origin + country.mountPath + "sitemap-root.xml")).append("</loc></sitemap>\n")
    country.cities.foreach { city =>
      sb.append("  <sitemap><loc>").append(escape(origin + CityPath(city) + "/sitemap.xml")).append("</loc></sitemap>\n")
    }
    sb.append("</sitemapindex>\n")
    sb.toString
  }

  /** The BRAND FRONT DOOR's `sitemap.xml`: a sitemap INDEX naming each country
   *  mounted under the apex, not a URL list of its own.
   *
   *  A crawler only ever reads `sitemap.xml` at a HOST's root, and the apex's
   *  root belongs to the country picker rather than to any one country — so the
   *  four Showtimes countries under `showtimes.cc/{code}/` have no other way to
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
