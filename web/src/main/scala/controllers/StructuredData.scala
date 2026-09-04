package controllers

import models.City
import play.api.libs.json.{JsArray, JsValue, Json}

import java.time.format.DateTimeFormatter

/** Builds the schema.org JSON-LD emitted in a `<script type="application/ld+json">`
 *  block (see `views.html._jsonLd`) on each public page. Pure (no I/O) so it
 *  unit-tests against fixed inputs; templates call it directly with the data +
 *  `City` already in scope, so no controller signature changes are needed.
 *
 *  Three page shapes:
 *    - landing  → WebSite + Organization
 *    - city     → BreadcrumbList + ItemList of the films on show
 *    - film     → Movie + BreadcrumbList + one ScreeningEvent per showtime
 *
 *  No `aggregateRating`: every score we show is somebody else's (IMDb, Filmweb,
 *  Metacritic, Rotten Tomatoes), and Google's review-snippet guidelines say
 *  "Don't aggregate reviews or ratings from other websites". Marking them up
 *  also demanded a ratingCount/reviewCount we don't hold and won't invent —
 *  Search Console flagged exactly that ("Either ratingCount or reviewCount
 *  should be specified") on 2026-09-02. The scores stay on the page for
 *  readers; they just aren't claimed as ours in the JSON-LD.
 */
object StructuredData {

  private val Ctx = "https://schema.org"

  /** The origin to attribute a render to when the caller has no request context
   *  (fixture/snapshot renders pass `""`). THIS DEPLOYMENT's own host, not a
   *  literal: a hardcoded Polish host put `kinowo.fly.dev` into the UK site's
   *  JSON-LD, telling Google the two were one site. */
  private def fallbackOrigin: String = models.Country.fromEnv.ogOrigin

  /** `scheme://host` from a full page URL, falling back to [[fallbackOrigin]]
   *  when the caller has no request context. */
  def originOf(pageUrl: String): String =
    if (pageUrl.isEmpty) fallbackOrigin
    else {
      val u = java.net.URI.create(pageUrl)
      if (u.getScheme == null || u.getAuthority == null) fallbackOrigin
      else s"${u.getScheme}://${u.getAuthority}"
    }

  /** Landing page: identify the site + publisher so Google can attach a
   *  knowledge-panel / sitelinks to the brand. Everything is the DEPLOYMENT's:
   *  its brand, its own host (so the UK site self-identifies as
   *  showtimes.cc/uk, not kinowo.net), its home montage, and its
   *  language's landing copy (reusing the `landing.ogDescription` message rather
   *  than a second, drift-prone Polish literal). */
  def landing()(implicit messages: play.api.i18n.Messages): String = {
    val country = models.Country.fromEnv
    val origin  = country.ogOrigin
    render(Json.arr(
      Json.obj(
        "@context" -> Ctx, "@type" -> "WebSite",
        "name" -> country.brandName, "url" -> s"$origin/",
        "inLanguage" -> country.language.getLanguage,
        "description" -> messages("landing.ogDescription"),
      ),
      Json.obj(
        "@context" -> Ctx, "@type" -> "Organization",
        "name" -> country.brandName, "url" -> s"$origin/",
        "logo" -> s"$origin/assets/img/${country.homeOgImage}",
      ),
    ))
  }

  /** A city listing (`/{slug}/` or `/{slug}/movies`): breadcrumb back to the
   *  landing plus an ItemList of the films currently on show, each linking to
   *  its detail page — a crawlable index of the city's long-tail URLs. */
  def cityPage(pageUrl: String, city: City, films: Seq[FilmSchedule]): String = {
    val origin   = originOf(pageUrl)
    val cityUrl  = s"$origin/${city.slug}/"
    // Distinct on the ASSIGNED slug rather than the title: same-titled films
    // are separate entries with separate URLs, and collapsing them by title
    // hid one of the two from the crawlable index.
    val entries = films.map(f => (f.slug, f.movie.title)).distinct.sortBy(_._2)
    val items = entries.zipWithIndex.map { case ((slug, title), i) =>
      Json.obj(
        "@type" -> "ListItem", "position" -> (i + 1),
        "url" -> (origin + FilmHref.forSlug(slug, title, city)), "name" -> title,
      )
    }
    render(Json.arr(
      breadcrumb(origin, Seq(city.country.brandName -> s"$origin/", city.labels.nominative -> cityUrl)),
      place(cityUrl, city),
      Json.obj(
        "@context" -> Ctx, "@type" -> "ItemList",
        // Same city heading as the OG tags / card overlay, so the JSON-LD is
        // in the deployment's language ("Repertuar kin w Poznaniu" /
        // "Cinema listings in London") rather than a half-Polish mix.
        "name" -> FilterDescription.cityHeading(city),
        "numberOfItems" -> items.size,
        "itemListElement" -> items,
      ),
    ))
  }

  /** The PLACE the listing is about, as a schema.org `City`: its coordinates,
   *  and — for a listing that spans several towns — those towns as
   *  `containsPlace`. Without it the only geography on a city page was its own
   *  name, in the title and the breadcrumb, so a page covering several towns
   *  named none of them anywhere a crawler could read: `/trojmiasto/` said
   *  "Trójmiasto" and never Gdynia, Sopot or Rumia, and a US metro said "New
   *  York" and never Brooklyn or Long Island.
   *
   *  A `City` rather than a bare `Place` because that is what every entry in
   *  [[models.City.coveredPlaces]] is — a town, or a district of one — and
   *  because `containsPlace` is the property that says so. */
  private def place(cityUrl: String, city: City): JsValue = {
    val towns = city.otherCoveredPlaces
    Json.obj(
      "@context" -> Ctx, "@type" -> "City",
      "name" -> city.labels.nominative,
      "url"  -> cityUrl,
      "geo"  -> Json.obj("@type" -> "GeoCoordinates", "latitude" -> city.lat, "longitude" -> city.lon),
    ) ++ (
      if (towns.isEmpty) Json.obj()
      else Json.obj("containsPlace" -> towns.map(t => Json.obj("@type" -> "City", "name" -> t)))
    )
  }

  /** A film detail page: the Movie itself + a breadcrumb + a ScreeningEvent per
   *  showtime (which makes the screenings eligible for Google's event surfaces). */
  def film(canonicalUrl: String, city: City, fs: FilmSchedule): String = {
    val origin  = originOf(canonicalUrl)
    val cityUrl = s"$origin/${city.slug}/"
    val m       = fs.movie

    // Merging an empty object adds no fields, so the optional pieces below drop
    // out cleanly when their data is absent.
    val movie = Json.obj("@context" -> Ctx, "@type" -> "Movie", "name" -> m.title, "url" -> canonicalUrl)
      .++(optStr("description", fs.synopsis))
      .++(optStr("image", absoluteImage(origin, fs)))
      .++(seqObj("genre", m.genres))
      .++(seqPersons("director", fs.director))
      .++(seqPersons("actor", fs.cast.take(15)))
      .++(m.releaseYear.fold(Json.obj())(y => Json.obj("dateCreated" -> y.toString)))
      .++(m.runtimeMinutes.fold(Json.obj())(min => Json.obj("duration" -> s"PT${min}M")))

    val events = fs.showings.flatMap { case (_, perCinema) =>
      perCinema.flatMap { cs =>
        cs.showtimes.map { st =>
          val start = st.dateTime.atZone(city.zoneId).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          Json.obj(
            "@context" -> Ctx, "@type" -> "ScreeningEvent",
            "name" -> m.title,
            "startDate" -> start,
            "url" -> st.bookingUrl.getOrElse(canonicalUrl),
            "location" -> Json.obj(
              "@type" -> "MovieTheater", "name" -> cs.cinema.displayName,
              "address" -> Json.obj(
                "@type" -> "PostalAddress",
                "addressLocality" -> city.labels.nominative,
                "addressCountry" -> city.country.language.getCountry,
              ),
            ),
            "workPresented" -> Json.obj("@type" -> "Movie", "name" -> m.title),
          )
        }
      }
    }

    val crumb = breadcrumb(origin, Seq(
      city.country.brandName -> s"$origin/", city.labels.nominative -> cityUrl, m.title -> canonicalUrl,
    ))

    render(JsArray(movie +: crumb +: events))
  }

  // ── helpers ──────────────────────────────────────────────────────────────

  private def breadcrumb(origin: String, crumbs: Seq[(String, String)]): JsValue =
    Json.obj(
      "@context" -> Ctx, "@type" -> "BreadcrumbList",
      "itemListElement" -> crumbs.zipWithIndex.map { case ((name, url), i) =>
        Json.obj("@type" -> "ListItem", "position" -> (i + 1), "name" -> name, "item" -> url)
      },
    )

  private def absoluteImage(origin: String, fs: FilmSchedule): Option[String] =
    fs.posterUrl.map { p =>
      if (p.startsWith("http://") || p.startsWith("https://")) p
      else origin + (if (p.startsWith("/")) p else "/" + p)
    }

  private def optStr(key: String, v: Option[String]) =
    v.filter(_.nonEmpty).fold(Json.obj())(s => Json.obj(key -> s))

  private def seqObj(key: String, vs: Seq[String]) =
    if (vs.isEmpty) Json.obj() else Json.obj(key -> vs)

  private def seqPersons(key: String, names: Seq[String]) =
    if (names.isEmpty) Json.obj()
    else Json.obj(key -> names.map(n => Json.obj("@type" -> "Person", "name" -> n)))

  /** Compact JSON, made safe to embed in a `<script>` block: a `</script>` (or
   *  any `</…`) inside a synopsis/title would otherwise close the tag early, so
   *  escape `<` — `<\/` is still valid JSON and renders identically. */
  private def render(v: JsValue): String =
    Json.stringify(v).replace("<", "\\u003c")
}
