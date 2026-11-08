package controllers

import models.{City, Country}

import java.time.Instant

/** The crawl files a deployment serves — `robots.txt` and the sitemaps — and
 *  which of their shapes a request wants: the brand front door (the shared apex)
 *  speaks for every country mounted under it, a country's own site only for itself.
 *
 *  Scoped to THIS deployment's country — a `KINOWO_COUNTRY=pl` (Poland) host must
 *  not advertise the UK/Germany cities that also live in the global `City.all`
 *  (those pages render empty on this host, so crawling them is pure waste). Each
 *  country's own deployment sitemaps its own cities. Same scope the landing +
 *  navbar use (the wiring's `country`).
 *
 *  `schedulesFor` is the warm read model's per-city join (cheap, in memory), so the
 *  sitemap always reflects what's actually live; `cityStamp` is the city's own
 *  read-model stamp. */
final class CrawlerFiles(servingCountry: Country,
                         schedulesFor: City => Seq[FilmSchedule],
                         cityStamp: City => Instant) {

  /** `robots.txt` — see [[RobotsTxt]] for what goes in it and why. */
  def robots(host: String, origin: String): String =
    if (servingCountry.servesApex(host)) RobotsTxt.frontDoor(mountedUnderApex)
    else RobotsTxt.forCountry(origin + servingCountry.pathPrefix, servingCountry)

  /** `sitemap.xml` — the full crawl map: landing, every city listing + plan, and
   *  every film each city is currently showing.
   *
   *  A country whose corpus crosses `SitemapBuilder.CityPartitionThreshold`
   *  URLs gets a sitemap INDEX here instead of the flat file — one sub-sitemap
   *  per city ([[citySitemap]]) plus one for the landing ([[sitemapRoot]]). Today
   *  that's only the US corpus, but the check is against the actual count, not
   *  a hardcoded country, so it holds as any corpus grows past the threshold.
   *
   *  `origin` is the bare ORIGIN: every `<loc>` picks the mount point up from the
   *  city (or, for the landing, from the country) via the same builders the pages
   *  themselves use, so a country sharing the brand domain neither drops the prefix
   *  nor doubles it. */
  def sitemap(host: String, origin: String): String =
    if (servingCountry.servesApex(host)) SitemapBuilder.index(mountedUnderApex)
    else {
      val entries = servingCountry.cities.map(c => c -> schedulesFor(c))
      if (SitemapBuilder.urlCount(entries) > SitemapBuilder.CityPartitionThreshold)
        SitemapBuilder.cityIndex(origin, servingCountry)
      else
        SitemapBuilder.build(origin, servingCountry, entries, lastmod = cityLastmod)
    }

  /** The landing-only sitemap a partitioned country's index
   *  ([[SitemapBuilder.cityIndex]]) points at, since the landing URL belongs to no
   *  one city and would look like that city's own page if it rode along in one
   *  city's sub-sitemap instead. */
  def sitemapRoot(origin: String): String =
    SitemapBuilder.build(origin, servingCountry, Nil)

  /** One city's slice of the full crawl map, served only once a country's flat
   *  `sitemap.xml` has been partitioned (see [[sitemap]]). Reuses the exact same
   *  per-city lastmod the flat file is stamped with; the only difference is the
   *  landing URL, which this omits — [[sitemapRoot]] already speaks for it. */
  def citySitemap(origin: String, city: City): String =
    SitemapBuilder.build(origin, servingCountry, Seq(city -> schedulesFor(city)),
                         lastmod = cityLastmod, includeLanding = false)

  /** The countries that share the brand domain, and so the ones the front door's
   *  `robots.txt` and `sitemap.xml` have to speak for: a crawler reads both only
   *  at a host's ROOT, which none of them owns. Poland is excluded by having no
   *  path prefix — it is a different host with a root of its own. */
  private def mountedUnderApex: Seq[Country] =
    Country.switchable.filter(_.pathPrefix.nonEmpty)

  /** The `<lastmod>` W3C date for `city`'s URLs — its own per-city read-model
   *  stamp (`WebReadModel.lastModifiedFor`), not the model-wide
   *  `readModel.lastModified`. The model-wide stamp moves on every city's
   *  change, so it claimed a Warsaw showtime edit as a change to every other
   *  city's URLs too — the same over-invalidation `lastModifiedFor` already
   *  exists to avoid for conditional GETs (see `ConditionalResponse`). */
  private def cityLastmod(city: City): Option[String] =
    Some(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE.format(cityStamp(city).atOffset(java.time.ZoneOffset.UTC)))
}

object CrawlerFiles {

  /** The crawler files of the live corpus: the warm read model's schedules, each city's own stamp. */
  def live(servingCountry: Country, schedules: MovieControllerService, readModel: services.readmodel.WebReadModel): CrawlerFiles =
    new CrawlerFiles(servingCountry, schedules.toSchedules(_), city => readModel.lastModifiedFor(city.slug))
}
