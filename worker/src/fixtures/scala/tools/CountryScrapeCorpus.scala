package tools

import models.{Cinema, CinemaMovie, Country, Movie, Showtime}

import java.time.LocalDateTime
import scala.util.Random

/**
 * A deterministic, country-shaped scrape corpus — one consolidated listing per
 * cinema, in the exact form a real client emits (`Seq[CinemaMovie]`), ready to be
 * written into `cinema_scrapes` and replayed through the pipeline.
 *
 * It exists because the recorded HTTP corpus is **Poland-only**
 * (`.github/scripts/record-country-fixture.sh` never sets `KINOWO_COUNTRY`, so
 * the recorder walks Poland's cities), which leaves the German and British
 * pipelines — different cinema catalogues, different country-scoped
 * `TitleNormalizer` rules — with no way to be replayed at all.
 *
 * This is NOT a substitute for a real-corpus replay: it asserts nothing about
 * which films exist. What it does is put each country's OWN catalogue and title
 * rules under the title shapes that have historically made the corpus oscillate,
 * so "does this country's pipeline reach a fixpoint" becomes an answerable
 * question. The shapes are drawn from bugs this repository actually shipped:
 *
 *   - a decorated edition beside its base film (programme prefix) — the
 *     "Plenerowe Pałacowe: Ścieżki życia" split that re-diverted every tick,
 *   - a `+ event` suffix, which must stay its own record,
 *   - dubbing / subtitle variants of ONE film at ONE cinema, which share the
 *     year-less slot key `CinemaShowing(cinema, sanitize(title))` — the same-slot
 *     ping-pong that re-fired a change event on every identical re-scrape,
 *   - a year embedded in the title, against the same film scraped year-less,
 *   - an ampersand title, whose normalisation rule is country-scoped,
 *   - a case/diacritic variant of a title another venue spells cleanly,
 *   - the same film across many venues, which is what creates merge pressure.
 *
 * Fully deterministic: the seed is derived from the country code, so a failure
 * reproduces exactly rather than as a flake.
 */
object CountryScrapeCorpus {

  /** Films per cinema. Kept small on purpose — the pipeline's cost is
   *  film×cinema pairs, and the corpus needs to be broad (every venue in the
   *  country) rather than deep. ~6 × the country's venue count lands all three
   *  countries in the same order of magnitude as the Polish HTTP replay. */
  private val FilmsPerCinema = 6

  /** Base titles the variants below decorate. Deliberately mundane and
   *  language-neutral — the corpus tests title MECHANICS, not vocabulary. */
  private val BaseTitles = Vector(
    "Ścieżki życia", "Nocny kurier", "Blue Harvest", "Der lange Sommer",
    "The Quiet Coast", "Anora", "Nosferatu", "Diuna", "Wicked", "Konklawe",
    "Sonic 3", "Vermiglio", "Grand Tour", "Flow", "September 5", "Babygirl"
  )

  private val Formats  = Vector(List("2D"), List("2D", "NAP"), List("IMAX", "2D"), List("3D"), Nil)
  private val Rooms    = Vector(Some("Sala 1"), Some("Sala 2"), Some("Screen 4"), Some("Saal 3"), None)

  /** How one cinema spells one base film. Several of these intentionally collide
   *  on the year-less slot key so the fold has something to do. */
  private sealed trait Variant { def render(base: String): String }
  private case object Plain        extends Variant { def render(b: String) = b }
  private case object Dubbed       extends Variant { def render(b: String) = s"$b (dubbing)" }
  private case object Subtitled    extends Variant { def render(b: String) = s"$b - napisy" }
  private case object Yeared       extends Variant { def render(b: String) = s"$b (2026)" }
  private case object Upper        extends Variant { def render(b: String) = b.toUpperCase(java.util.Locale.ROOT) }
  private case object Programme    extends Variant { def render(b: String) = s"Plenerowe Pałacowe: $b" }
  private case object PlusEvent    extends Variant { def render(b: String) = s"$b + spotkanie z twórcami" }
  private case object Ampersand    extends Variant { def render(b: String) = s"$b & przyjaciele" }

  private val Variants = Vector(Plain, Dubbed, Subtitled, Yeared, Upper, Programme, PlusEvent, Ampersand)

  /** Every cinema this country actually serves, in catalogue order. */
  def cinemasOf(country: Country): Seq[Cinema] = country.cities.flatMap(_.cinemas).distinct

  /** One listing per cinema for `country`. The same base films recur across
   *  venues (that is what makes the corpus a merge problem rather than a set of
   *  independent rows), each venue spelling them its own way. */
  def listings(country: Country, day: LocalDateTime): Map[Cinema, Seq[CinemaMovie]] = {
    val cinemas = cinemasOf(country)
    val random  = new Random(seedFor(country))

    cinemas.zipWithIndex.map { case (cinema, cinemaIndex) =>
      // Rotate the base-title window per venue so neighbouring cinemas overlap
      // heavily but not identically — films spread across the catalogue the way
      // a real week's repertoire does.
      val films = (0 until FilmsPerCinema).flatMap { slot =>
        val base    = BaseTitles((cinemaIndex * 3 + slot) % BaseTitles.size)
        val variant = Variants((cinemaIndex + slot * 5) % Variants.size)
        val primary = film(cinema, variant.render(base), random)
        // Every fourth (cinema, film) also lists the SAME film under a second
        // spelling at the SAME venue — the year-less-slot collision that the
        // re-scrape ping-pong rode in on. One cinema, one film, two titles.
        if ((cinemaIndex + slot) % 4 == 0) Seq(primary, film(cinema, Dubbed.render(base), random))
        else Seq(primary)
      }
      cinema -> films
    }.toMap
  }

  /** A seed pinned to the country, so each leg is reproducible on its own and
   *  two countries never generate the identical stream. */
  private def seedFor(country: Country): Long =
    country.code.foldLeft(0x5DEECE66DL)((acc, ch) => acc * 31 + ch.toLong)

  private def film(cinema: Cinema, title: String, random: Random): CinemaMovie = {
    val showtimeCount = 1 + random.nextInt(4)
    CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = Some(90 + random.nextInt(60)),
        // Deliberately mixed: a yearless film beside a yeared one is the
        // disagreement the canonical key has to settle.
        releaseYear    = if (random.nextBoolean()) Some(2026) else None,
        countries      = Seq("USA"),
        genres         = Seq("Dramat"),
        originalTitle  = None,
        rawTitle       = Some(title)
      ),
      cinema      = cinema,
      posterUrl   = Some(s"https://poster.test/${slug(title)}.jpg"),
      filmUrl     = Some(s"https://${slug(cinema.displayName)}.test/film/${slug(title)}"),
      synopsis    = Some(s"Opis filmu $title."),
      cast        = Seq("Actor One", "Actor Two"),
      director    = Seq("Some Director"),
      showtimes   = (0 until showtimeCount).map { i =>
        Showtime(
          dateTime   = LocalDateTime.of(2026, 8, 1, 10, 0).plusHours(i * 3L).plusDays(i % 3L),
          bookingUrl = Some(s"https://${slug(cinema.displayName)}.test/book/${slug(title)}/$i"),
          room       = Rooms((i + title.length) % Rooms.size),
          format     = Formats((i + title.length) % Formats.size)
        )
      },
      externalIds = Map.empty,
      trailerUrl  = None,
      ageRating   = None
    )
  }

  private def slug(value: String): String =
    java.text.Normalizer.normalize(value, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .replace("ł", "l").replace("Ł", "L")
      .toLowerCase(java.util.Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "-")
      .stripPrefix("-").stripSuffix("-")
}
