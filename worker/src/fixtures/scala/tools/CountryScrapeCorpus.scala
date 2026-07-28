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

  /** The shape of a country's repertoire, as `(films per venue, distinct films
   *  per 10 venues)` — both measured off production on 2026-07-28 via
   *  `/api/repertoire`:
   *
   *    PL   274 venues, 1,129 films, 6,510 pairs -> 24 per venue, 4.1 films/venue
   *    DE 1,083 venues, 1,161 films, 13,248 pairs -> 12 per venue, 1.1 films/venue
   *    UK   778 venues, 1,695 films, 24,156 pairs -> 31 per venue, 2.2 films/venue
   *
   *  Both are RATIOS rather than counts, so the corpus grows and shrinks with the
   *  catalogue instead of being pinned to a constant — the whole reason a fixed
   *  title list was wrong. The two numbers are very different per country (a
   *  German venue lists half what a British one does, and Germany's 1,083 venues
   *  share barely more films than Poland's 274) and using one country's shape for
   *  all three both misrepresents the others and, for Germany, generated five
   *  times production's film count and a 27-minute run. */
  private def shapeOf(country: Country): (Int, Int) = country match {
    case Country.Germany       => (12, 11)
    case Country.UnitedKingdom => (31, 22)
    case _                     => (24, 41)
  }

  /** Films each venue lists. */
  private def filmsPerCinema(country: Country): Int = shapeOf(country)._1

  /** How many DISTINCT films the country's repertoire holds. Derived from the
   *  catalogue, never a constant: a fixed pool is saturated by the first few
   *  hundred venues, after which every country settles to the same number of
   *  films however many cinemas it has — which is the tell that the figure
   *  describes the generator rather than the country. */
  private def titlePool(country: Country): Int =
    // Halved, because the pool counts BASE titles and each base settles as TWO
    // films: itself (which every spelling but one folds into) and its ampersand
    // edition, which is a genuinely different title and rightly keeps its own row.
    // Sizing the pool at the measured film count therefore produced twice
    // production's corpus — 3,382 UK films against 1,695, 1,869 Polish against
    // 1,129 — and the surplus is pure runtime in the fold, projection and render.
    math.max(1, cinemasOf(country).size * shapeOf(country)._2 / 20)

  // Titles are COMPOSED, not listed, so the pool can be however large the country
  // needs. 24 x 24 x 8 = 4,608 distinct titles — comfortably past the largest
  // catalogue — while staying mundane and language-mixed, since what is under test
  // is title MECHANICS and not vocabulary.
  private val Openers = Vector(
    "Cicha", "Ostatnia", "Zimna", "Nocny", "Blue", "Long", "Perfect", "Northern",
    "Grand", "Wielka", "Der lange", "Das letzte", "Quiet", "Green", "Salt", "Iron",
    "Zielona", "Czarna", "Biały", "Srebrny", "Golden", "Silent", "Distant", "Hidden")
  private val Subjects = Vector(
    "noc", "seans", "wojna", "kurier", "Harvest", "Walk", "Days", "Lights",
    "Tour", "podróż", "Sommer", "Boot", "Coast", "Border", "Path", "Horizon",
    "granica", "ścieżka", "dom", "ogród", "River", "Winter", "Signal", "Garden")
  private val Qualifiers = Vector(
    "", " II", " III", ": Powrót", ": Początek", ": Epilog", " Reloaded", ": Finale")

  /** The `n`th title of the pool. Pure — the same index is the same film in every
   *  pass and every country, which is what lets a venue's listing be regenerated
   *  rather than stored. */
  private def titleAt(n: Int): String = {
    val opener    = Openers(Math.floorMod(n, Openers.size))
    val subject   = Subjects(Math.floorMod(n / Openers.size, Subjects.size))
    val qualifier = Qualifiers(Math.floorMod(n / (Openers.size * Subjects.size), Qualifiers.size))
    s"$opener $subject$qualifier"
  }

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
    val pool    = titlePool(country)
    val perVenue = filmsPerCinema(country)
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
      val films = (0 until perVenue).flatMap { slot =>
        val base    = titleAt(Math.floorMod(mix(cinemaIndex, slot), pool))
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
