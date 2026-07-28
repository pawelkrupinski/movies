package tools

import models.{Cinema, CinemaMovie, Country, Movie, Showtime}

import java.time.LocalDateTime

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
 * Fully deterministic by CONSTRUCTION — every field is a pure function of
 * (film, venue), with no RNG whose consumption order could matter — so a failure
 * reproduces exactly rather than as a flake.
 */
object CountryScrapeCorpus {

  /** Films per cinema. Kept small on purpose — the pipeline's cost is
   *  film×cinema pairs, and the corpus needs to be broad (every venue in the
   *  country) rather than deep. ~6 × the country's venue count lands all three
   *  countries in the same order of magnitude as the Polish HTTP replay. */
  private val FilmsPerCinema = 6

  /** Base titles the variants below decorate. Deliberately mundane and
   *  language-neutral — the corpus tests title MECHANICS, not vocabulary — but
   *  broad enough that the settled corpus isn't a handful of rows every venue
   *  piles onto. */
  private val BaseTitles = Vector(
    "Ścieżki życia", "Nocny kurier", "Blue Harvest", "Der lange Sommer",
    "The Quiet Coast", "Anora", "Nosferatu", "Diuna", "Wicked", "Konklawe",
    "Sonic 3", "Vermiglio", "Grand Tour", "Flow", "September 5", "Babygirl",
    "Zimna wojna", "Ostatni seans", "Harvest Moon", "Die Blechtrommel",
    "The Long Walk Home", "Perfect Days", "Past Lives", "Aftersun",
    "Cicha noc", "Zielona granica", "Broker", "Drive My Car",
    "Der Vorleser", "Das Boot", "Northern Lights", "The Salt Path",
    "Chłopi", "Kos", "Iluzja", "Fremont",
    "La Chimera", "Tár", "Saltburn", "Poor Things"
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
    cinemas.zipWithIndex.map { case (cinema, cinemaIndex) =>
      // Which film, and how this venue spells it, both come from a MIXING hash of
      // (venue, slot) rather than from arithmetic on the indices. The arithmetic
      // version — `(3c+s) % 16` and `(c+5s) % 8` — silently locked the two
      // together: both reduce to `(c+s) mod 2`, so a variant only ever landed on
      // base titles of its own parity. Half the (title, spelling) combinations
      // were unreachable, the reachable half saturated after ~32 venues, and every
      // country therefore settled to the SAME 24 films no matter how many cinemas
      // it had — a number that looked like a pipeline invariant and was really an
      // artefact of the generator. Mixed, all combinations occur.
      val films = (0 until FilmsPerCinema).flatMap { slot =>
        val base    = BaseTitles(Math.floorMod(mix(cinemaIndex, slot), BaseTitles.size))
        val variant = Variants(Math.floorMod(mix(slot * 31 + 7, cinemaIndex) >>> 3, Variants.size))
        val primary = film(cinema, cinemaIndex, base, variant.render(base))
        // Every fourth (cinema, film) also lists the SAME film under a second
        // spelling at the SAME venue — the year-less-slot collision that the
        // re-scrape ping-pong rode in on. One cinema, one film, two titles.
        if ((cinemaIndex + slot) % 4 == 0) Seq(primary, film(cinema, cinemaIndex, base, Dubbed.render(base)))
        else Seq(primary)
      }
      cinema -> films
    }.toMap
  }

  /** Mixing hash over two small integers — enough avalanche that low bits of the
   *  inputs don't survive into the output, which is what the old index arithmetic
   *  got wrong. Pure Int arithmetic (wrapping, like the reference implementation),
   *  so it is identical on every JVM and the corpus stays reproducible. */
  private def mix(a: Int, b: Int): Int = {
    var h = a * 0x9E3779B1
    h ^= (b + 0x85EBCA6B) + (h << 6) + (h >>> 2)
    h ^= h >>> 15
    h * 0x27D4EB2F
  }

  /** One film as one cinema lists it.
   *
   *  Everything except the TITLE is derived from the BASE film, not from the
   *  spelling and not from a running RNG. That is deliberate and load-bearing: a
   *  venue that lists the same film twice (the dub twin below) puts both rows on
   *  ONE year-less slot key, so if the two carried different runtimes the winner
   *  would be whichever arrived last — manufacturing an order dependency that no
   *  real corpus has, and that the order-independence spec would then report as a
   *  pipeline fault. A film has one runtime everywhere; only how a venue SPELLS
   *  it varies.
   *
   *  The year is the one field allowed to disagree, and only ACROSS venues: some
   *  cinemas publish it and some don't, which is exactly the yearless-into-yeared
   *  fold the settle has to get right. Both twins at a venue agree, because it is
   *  keyed on (film, venue). */
  private def film(cinema: Cinema, cinemaIndex: Int, base: String, title: String): CinemaMovie = {
    val baseKey       = base.hashCode
    val showtimeCount = 1 + Math.floorMod(mix(cinemaIndex, baseKey), 4)
    CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = Some(80 + Math.floorMod(mix(baseKey, 0), 80)),
        releaseYear    = Option.when(Math.floorMod(mix(baseKey, cinemaIndex + 1), 3) != 0)(2026),
        countries      = Seq("USA"),
        genres         = Seq("Dramat"),
        originalTitle  = None,
        rawTitle       = Some(title)
      ),
      cinema      = cinema,
      posterUrl   = Some(s"https://poster.test/${slug(base)}.jpg"),
      filmUrl     = Some(s"https://${slug(cinema.displayName)}.test/film/${slug(base)}"),
      synopsis    = Some(s"Opis filmu $base."),
      cast        = Seq("Actor One", "Actor Two"),
      director    = Seq("Some Director"),
      showtimes   = (0 until showtimeCount).map { i =>
        Showtime(
          dateTime   = LocalDateTime.of(2026, 8, 1, 10, 0).plusHours(i * 3L).plusDays(i % 3L),
          bookingUrl = Some(s"https://${slug(cinema.displayName)}.test/book/${slug(base)}/$i"),
          room       = Rooms(Math.floorMod(mix(baseKey, i), Rooms.size)),
          format     = Formats(Math.floorMod(mix(i, baseKey), Formats.size))
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
