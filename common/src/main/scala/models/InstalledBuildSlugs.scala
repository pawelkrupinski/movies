package models

/**
 * TEMPORARY, with [[LegacySlugOwners]] and dropped by the same rule: the slug
 * installed app builds from before slugs were per country know each city by.
 *
 * Those builds (`mobile-ios-2.0.11`, `mobile-android-2.0.11`) treat the catalog's
 * `slug` as globally unique: they look a city's country and zone up by the first
 * city with that slug, save the bare slug as the selected city, and build their
 * API URLs as `{selected country's baseUrl}/{slug}/api/…`. Since a slug became
 * per country, 42 of them are pages of two countries at once (`york` is the UK's
 * and Pennsylvania's), and for those builds two catalog rows with one slug would
 * send a UK York user to Pennsylvania's zone and a Pennsylvania deep link to the
 * UK; and three US pages (`kent`, `somerset`, `suffolk`) now answer at slugs
 * those builds saved as the UK's retired counties.
 *
 * So the catalog's `slug` stays what those builds can tell apart. A page keeps its
 * own slug unless [[LegacySlugOwners]] names ANOTHER country as that slug's old
 * owner — Pennsylvania's `york` over the UK's new York page, the UK's retired
 * `kent` county over Kent, Washington (which old builds knew as
 * `kent-washington`) — or, for a slug the table does not list, unless the slug is
 * a page of an earlier country in [[Country.switchable]] order too, the country a
 * link naming no country goes to ([[Country.bareSlugTarget]]). Such a page is
 * known by its QUALIFIED slug (`york-north-yorkshire`), which 301s to it on its
 * own country, API sub-paths included — so a request an old build makes lands
 * where it means. The page's own, per-country slug rides alongside as
 * `pageSlug`, which current builds read instead.
 *
 * Drop this, `pageSlug`'s fallback in the apps, and the AASA excludes built from
 * [[displaced]] once the minimum-version gate has retired those builds.
 */
object InstalledBuildSlugs {

  /** The country that keeps `slug` bare: its old owner, else the first country
   *  with a page at it. */
  private lazy val keeper: Map[String, Country] = {
    val firstPage = Country.switchable.reverse.flatMap(c => c.cities.map(_.slug -> c)).toMap
    firstPage ++ LegacySlugOwners.owners.flatMap { case (slug, code) => Country.byCode(code).map(slug -> _) }
  }

  /** (country, page slug) → the qualified slug installed builds know that page by,
   *  for every page whose slug another country keeps bare. */
  private lazy val qualified: Map[(Country, String), String] =
    qualifiedSlugs(Country.switchable, keeper, CitySlugIndexData.sharedPageQualifiedSlugs)

  /** [[qualified]] over `countries`, `keeper` and the generated per-country `table`.
   *  Total: a page another country keeps bare but `table` has no qualified slug for
   *  keeps its own. That is an old owner whose slug is now only an ALIAS of one of its
   *  pages — `generate_slug_index.py` qualifies a page only where another country has
   *  the slug as a page or a retired page — and with no qualified slug to answer at,
   *  the page's own is the only one that opens it; throwing would fail [[Catalog]]
   *  at load instead. */
  private[models] def qualifiedSlugs(
    countries: Seq[Country],
    keeper: Map[String, Country],
    table: Map[String, Map[String, String]],
  ): Map[(Country, String), String] =
    (for {
      country   <- countries
      city      <- country.cities if !keeper.get(city.slug).contains(country)
      qualified <- table.get(country.code).flatMap(_.get(city.slug))
    } yield (country, city.slug) -> qualified).toMap

  /** The slug installed builds know `city` of `country` by — its own, unless
   *  another country keeps that one bare. */
  def of(country: Country, city: City): String = qualified.getOrElse((country, city.slug), city.slug)

  /** The pages installed builds do NOT know by their own slug, with their country:
   *  `/{country}/{page slug}/` names a different city to them (the keeper's), or
   *  none. Sorted by country order, then slug. */
  lazy val displaced: Seq[(Country, City)] =
    Country.switchable.flatMap(c => c.cities.filter(city => qualified.contains((c, city.slug))).sortBy(_.slug).map(c -> _))

  /** Catalog `slug` → the page it names, with its country: what an installed build
   *  opens for a link whose city segment is that slug, whatever country the link
   *  names (it drops the country segment and takes the first city with the slug). */
  lazy val opens: Map[String, (Country, City)] =
    Country.switchable.flatMap(c => c.cities.map(city => of(c, city) -> (c, city))).toMap

  /** Every city segment of every country — a page's slug, a renamed one, an alias
   *  ([[Country.resolve]]) — whose link an installed build opens on a city OTHER
   *  than the one the web sends it to: a [[displaced]] page, and an alias or
   *  renamed slug that is another country's catalog `slug` (`/uk/rochester/`, a UK
   *  town on Chatham's page, is Rochester, New York's `slug`). The iOS association
   *  file excludes `/{country}/{segment}/` and everything under it, so old builds
   *  open those links in Safari, on the right page. A segment no city of the
   *  catalog has opens nothing at all, which is not a wrong city, and is claimed.
   *  Sorted by country order, then segment. */
  lazy val misopened: Seq[(Country, String)] =
    Country.switchable.flatMap { c =>
      (c.cities.map(_.slug) ++ c.renamedSlugs.keys ++ c.slugAliases.keys).distinct.sorted.filter { segment =>
        val opened = opens.get(segment)
        qualified.contains((c, segment)) || (opened.isDefined && opened != c.resolve(segment).map(c -> _))
      }.map(c -> _)
    }
}
