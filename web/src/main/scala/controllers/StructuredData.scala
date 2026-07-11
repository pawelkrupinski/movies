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
 *    - film     → Movie (+ aggregateRating) + BreadcrumbList + one ScreeningEvent
 *                 per showtime
 *
 *  `aggregateRating` carries the honest ratingValue/best/worst only — we don't
 *  store a public rating COUNT, and inventing one to satisfy Google's
 *  rich-result eligibility would violate the structured-data guidelines. The
 *  rating is valid schema.org; it just may not light up the stars snippet.
 */
object StructuredData {

  private val ProdOrigin = "https://kinowo.fly.dev"
  private val Ctx        = "https://schema.org"

  /** `scheme://host` from a full page URL, falling back to the prod origin when
   *  the caller has no request context (fixture/snapshot renders pass `""`). */
  def originOf(pageUrl: String): String =
    if (pageUrl.isEmpty) ProdOrigin
    else {
      val u = java.net.URI.create(pageUrl)
      if (u.getScheme == null || u.getAuthority == null) ProdOrigin
      else s"${u.getScheme}://${u.getAuthority}"
    }

  /** Landing page: identify the site + publisher so Google can attach a
   *  knowledge-panel / sitelinks to the brand. */
  def landing(): String = {
    val brand = models.Country.fromEnv.brandName
    render(Json.arr(
      Json.obj(
        "@context" -> Ctx, "@type" -> "WebSite",
        "name" -> brand, "url" -> s"$ProdOrigin/",
        "inLanguage" -> models.Country.fromEnv.language.getLanguage,
        "description" -> "Repertuar kin w polskich miastach — godziny seansów, oceny IMDb, Filmweb, Metacritic i Rotten Tomatoes.",
      ),
      Json.obj(
        "@context" -> Ctx, "@type" -> "Organization",
        "name" -> brand, "url" -> s"$ProdOrigin/",
        "logo" -> s"$ProdOrigin/assets/img/og-home.png",
      ),
    ))
  }

  /** A city listing (`/{slug}/` or `/{slug}/filmy`): breadcrumb back to the
   *  landing plus an ItemList of the films currently on show, each linking to
   *  its detail page — a crawlable index of the city's long-tail URLs. */
  def cityPage(pageUrl: String, city: City, films: Seq[FilmSchedule]): String = {
    val origin   = originOf(pageUrl)
    val cityUrl  = s"$origin/${city.slug}/"
    val titles   = films.map(_.movie.title).distinct.sorted
    val items = titles.zipWithIndex.map { case (title, i) =>
      Json.obj(
        "@type" -> "ListItem", "position" -> (i + 1),
        "url" -> (origin + FilmHref(title, city)), "name" -> title,
      )
    }
    render(Json.arr(
      breadcrumb(origin, Seq(city.country.brandName -> s"$origin/", city.labels.nominative -> cityUrl)),
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
      .++(aggregateRating(fs).fold(Json.obj())(r => Json.obj("aggregateRating" -> r)))

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

  /** IMDb first, then Filmweb (both 0–10), then Metacritic / Rotten Tomatoes
   *  (0–100). No ratingCount — we hold no public count and won't fabricate one. */
  private def aggregateRating(fs: FilmSchedule): Option[JsValue] = {
    val r = fs.resolved.ratings
    val picked: Option[(Double, Int)] =
      r.imdb.map(_ -> 10)
        .orElse(r.filmweb.map(_ -> 10))
        .orElse(r.metascore.map(_.toDouble -> 100))
        .orElse(r.rottenTomatoes.map(_.toDouble -> 100))
    picked.map { case (value, best) =>
      Json.obj(
        "@type" -> "AggregateRating",
        "ratingValue" -> value, "bestRating" -> best, "worstRating" -> (if (best == 10) 1 else 0),
      )
    }
  }

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
