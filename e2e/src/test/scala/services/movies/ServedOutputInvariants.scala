package services.movies

import controllers.{FilmSchedule, MovieControllerService}
import models.{City, Country, MovieRecord, Tmdb}
import services.cinemas.CountryNames
import services.readmodel.ReadModelProjection

import java.net.URI
import java.time.LocalDateTime
import java.util.Locale
import scala.util.Try

/** One card a city's `/` page serves, with the stored record it was projected from — the record
 *  carries what the card does not show but its links and figures must agree with (the tmdbId, the
 *  imdbId, TMDB's own release year) — and how many cards that record projects ([[ReadModelProjection]]'s
 *  display-title split: a dub or a programme variant of one film is a card of its own). */
final case class ServedCard(city: City, schedule: FilmSchedule, record: Option[MovieRecord], recordId: Option[String], variants: Int) {
  def title: String = schedule.movie.title
}

/** What one served card breaks: the city it is served in, its display title, and the rule. */
final case class ServedFinding(city: String, title: String, finding: String)

/** An allowlisted card: the city's slug (or [[AllowedCard.AnyCity]]) and its display title. */
final case class AllowedCard(city: String, title: String)

object AllowedCard {
  val AnyCity = "*"
  def anywhere(title: String): AllowedCard = AllowedCard(AnyCity, title)
}

/**
 * What every card a user is served must be, over a whole served corpus — the OUTPUT-side twin of
 * the corpus-shape specs, which hold each listing a parser hands the pipeline to its shape.
 *
 * The class of failure: a card that renders, with every field present, and is wrong in a way no
 * spec of one stage sees — two cards of one film in one city, a showtime listed twice, a booking
 * link a browser cannot follow, an IMDb score linked to another film, a year TMDB disagrees with
 * by a decade, a "2D napisy" or a "reż." credit left in the title, "Niderlandy" beside "Holandia". Each stage's own
 * spec passes over the film its author looked at; these rules read every card of every city.
 *
 * Pure: the caller builds the cards ([[cardsOf]]) from a booted wiring, so the rules are tested
 * over hand-built cards and run unchanged over the Polish fixture boot and each country's
 * convergence boot.
 */
object ServedOutputInvariants {

  /** Every city's served cards at `now`, each joined to the stored record it was projected from. */
  def cardsOf(wiring: tools.TestWiring, country: Country, normalizer: TitleNormalizer, now: LocalDateTime): Seq[ServedCard] = {
    val byCard: Map[String, (String, MovieRecord, Int)] = wiring.movieRepository.findAll().flatMap { stored =>
      val ids = ReadModelProjection.filmIds(stored, normalizer)
      ids.map(_ -> (stored.id.value, stored.record, ids.size))
    }.toMap
    val service = new MovieControllerService(wiring.webReadModel, wiring.clock)
    country.cities.sortBy(_.slug).flatMap { city =>
      service.toSchedules(city, now).map { schedule =>
        val joined = byCard.get(schedule.resolved._id)
        ServedCard(city, schedule, joined.map(_._2), joined.map(_._1), joined.fold(1)(_._3))
      }
    }
  }

  /** How much of `cards` the record-reading rules saw: joined to a stored film, and of those with a
   *  tmdbId — logged beside a verdict so a join that silently found nothing is visible. */
  def coverage(cards: Seq[ServedCard]): String = {
    val joined = cards.count(_.record.isDefined)
    s"${cards.size} card(s), $joined joined to their stored film, ${cards.count(_.record.exists(_.tmdbId.isDefined))} with a tmdbId"
  }

  /** Each rule `cards` (one country's) break. */
  def violations(country: Country, normalizer: TitleNormalizer, cards: Seq[ServedCard]): Seq[ServedFinding] =
    cards.flatMap(card => perCard(country, normalizer, card).map(ServedFinding(card.city.slug, card.title, _))) ++
      perCity(cards)

  // ── per city ─────────────────────────────────────────────────────────────

  private def perCity(cards: Seq[ServedCard]): Seq[ServedFinding] =
    cards.groupBy(_.city.slug).toSeq.flatMap { case (city, inCity) =>
      // Two cards of one film: allowed only as the read model's display-title split of ONE record
      // (a dub, a programme or an audience variant — memory project_split_rows_intentional: kept
      // apart by design). Two RECORDS holding one tmdbId is one film served twice.
      val sharedTmdb = inCity.filter(_.record.exists(_.tmdbId.isDefined)).groupBy(_.record.flatMap(_.tmdbId)).toSeq.flatMap {
        case (Some(tmdbId), same) if same.flatMap(_.recordId).distinct.sizeIs > 1 =>
          val titles = same.map(_.title).distinct.sorted.mkString(" | ")
          same.map(c => ServedFinding(city, c.title, s"tmdb-shared-across-records: $tmdbId ($titles)"))
        case _ => Nil
      }
      val sharedSlug = inCity.flatMap(c => c.schedule.slug.map(_ -> c)).groupBy(_._1).toSeq.collect {
        case (slug, same) if same.sizeIs > 1 => same.map { case (_, c) => ServedFinding(city, c.title, s"slug-shared: $slug") }
      }.flatten
      sharedTmdb ++ sharedSlug
    }

  // ── per card ─────────────────────────────────────────────────────────────

  private val ImdbId = """tt\d{7,}""".r
  /** A display title's year is the film's; a re-release, a restoration or a festival print may
   *  bill a year or two off TMDB's, never more. */
  private val YearTolerance = 2

  private def perCard(country: Country, normalizer: TitleNormalizer, card: ServedCard): Seq[String] = {
    val s = card.schedule
    val r = s.resolved.ratings
    val record = card.record
    val title = s.movie.title.trim
    // A card no stored film projects is one nothing upstream owns any more — and the rules below
    // that read the record (tmdbId, imdbId, TMDB's year) would hold over it by reading nothing.
    val orphan = Option.when(record.isEmpty)("card-without-stored-film").toSeq
    val titleFindings =
      (if (title.isEmpty) Seq("title-empty") else Nil) ++
        // A split variant's title carries the marker that MAKES it the variant ("… ukraiński dubbing");
        // only the plain card of a film is held to a marker-free title.
        Option.when(card.variants == 1)(displayMarkers(country, normalizer, title)).filter(_.nonEmpty)
          .map(m => s"title-carries-marker: ${m.mkString(", ")}")

    val showings = s.showings.flatMap { case (_, runs) => runs.flatMap(run => run.showtimes.map(run.cinema.displayName -> _)) }
    val emptyRuns = s.showings.flatMap(_._2).filter(_.showtimes.isEmpty).map(run => s"showing-without-showtime: ${run.cinema.displayName}")
    val noShowing = Option.when(showings.isEmpty)("card-without-showtime").toSeq
    // One screening printed twice: a slot (start, room, format) at a venue holding more showtimes than
    // distinct specific booking links — a parallel screen sells under its own (`SlotFields.showtimes`).
    val duplicateShowtimes = showings.groupBy { case (cinema, st) => (cinema, st.dateTime, st.room, st.format) }
      .collect { case ((cinema, at, room, _), same) if same.sizeIs > same.flatMap(_._2.bookingUrl).filter(SlotFields.specific).distinct.size.max(1) =>
        s"showtime-repeated: $cinema $at${room.fold("")(" " + _)} ×${same.size}" }
      .toSeq.sorted

    val urls =
      s.posterUrl.filterNot(followable).map(u => s"poster-url-unfollowable: $u").toSeq ++
        s.resolved.fallbackPosterUrls.filterNot(followable).map(u => s"fallback-poster-url-unfollowable: $u") ++
        s.cinemaFilmUrls.collect { case (c, u) if !followable(u) => s"film-url-unfollowable: ${c.displayName} $u" } ++
        showings.flatMap { case (cinema, st) => st.bookingUrl.filterNot(followable).map(u => s"booking-url-unfollowable: $cinema $u") }.distinct ++
        s.resolved.trailerUrls.filterNot(followable).map(u => s"trailer-url-unfollowable: $u")

    val ratings =
      r.imdb.filterNot(v => v >= 1 && v <= 10).map(v => s"imdb-out-of-range: $v").toSeq ++
        r.metascore.filterNot(v => v >= 0 && v <= 100).map(v => s"metascore-out-of-range: $v") ++
        r.rottenTomatoes.filterNot(v => v >= 0 && v <= 100).map(v => s"rotten-tomatoes-out-of-range: $v") ++
        r.filmweb.filterNot(v => v >= 1 && v <= 10).map(v => s"filmweb-out-of-range: $v") ++
        Option.when(r.imdb.isDefined && r.imdbUrl.isEmpty)("imdb-rating-without-link") ++
        r.imdbUrl.filter(_ => record.isDefined).filterNot(u => record.flatMap(_.imdbUrl).contains(u)).map(u => s"imdb-link-not-the-film's: $u (imdbId ${record.flatMap(_.imdbId).getOrElse("—")})") ++
        record.flatMap(_.imdbId).filterNot(ImdbId.matches).map(id => s"imdb-id-malformed: $id") ++
        siteLink("metacritic", r.metascore, r.metacriticUrl, "www.metacritic.com", "/movie/") ++
        siteLink("rotten-tomatoes", r.rottenTomatoes, r.rottenTomatoesUrl, "www.rottentomatoes.com", "/m/") ++
        (if (country.filmwebEnabled) siteLink("filmweb", r.filmweb, r.filmwebUrl, "www.filmweb.pl", "/")
         else Option.when(r.filmweb.isDefined || r.filmwebUrl.nonEmpty)("filmweb-served-outside-poland").toSeq)

    val tmdbYear = record.flatMap(_.data.get(Tmdb)).flatMap(_.releaseYear)
    val year = for { shown <- s.movie.releaseYear; tmdb <- tmdbYear if (shown - tmdb).abs > YearTolerance }
      yield s"year-far-from-tmdb: $shown (TMDB $tmdb)"

    orphan ++ titleFindings ++ noShowing ++ emptyRuns ++ duplicateShowtimes ++ urls ++ ratings ++ year ++
      names("country", s.movie.countries, countryProblem(country.language)) ++
      names("genre", s.movie.genres, genreProblem(country.language))
  }

  private def followable(url: String): Boolean = SlotFields.followable(url)

  /** The screening markers a card's display title may not carry: a format or version word
   *  (`FormatTags`' vocabulary — "2D", "napisy", "OmU", "VOSE", "4K"), a "reż." credit, a trailing "PL".
   *  Not the premiere / special-screening / programme words the search strips: a title rule that
   *  shapes only the query leaves those in the display on purpose, so an event or a programme row
   *  keeps its own card (memory project_split_rows_intentional) — nor a programme prefix's own words
   *  ("(4DX Rewind) Twisters" is Cineworld's strand, a row of its own by design). */
  private def displayMarkers(country: Country, normalizer: TitleNormalizer, title: String): Seq[String] =
    SearchQueryMarkersSpec.markersIn(country, normalizer.programmePrefix(title).fold(title)(title.stripPrefix)).filter(m => FormatTags.FormatToken.contains(m) || m == "reż." || m == "trailing PL")

  /** A rating site's link: on that site, and — when the card shows the site's score — the film's
   *  own page, never the search page a card without a known page links to. */
  private def siteLink(site: String, score: Option[?], url: String, host: String, filmPath: String): Seq[String] = {
    val parsed = Try(new URI(url)).toOption
    val onSite = parsed.flatMap(u => Option(u.getHost)).exists(_.equalsIgnoreCase(host))
    if (!onSite) Seq(s"$site-link-off-site: $url")
    else Option.when(score.isDefined && !parsed.flatMap(u => Option(u.getPath)).exists(p => p.startsWith(filmPath) && !p.startsWith("/search")))(
      s"$site-score-with-search-link: $url").toSeq
  }

  private def names(what: String, values: Seq[String], problem: String => Option[String]): Seq[String] =
    values.flatMap(v => problem(v).map(p => s"$what-$p: $v")) ++
      values.groupBy(_.trim.toLowerCase(Locale.ROOT)).collect { case (_, same) if same.sizeIs > 1 => s"$what-repeated: ${same.head}" }

  /** A served country name is the language's canonical spelling: the one `CountryNames` folds
   *  every spelling to ("Holandia", never "Niderlandy"), and never another served language's. */
  private def countryProblem(language: Locale)(name: String): Option[String] = {
    val canonical = CountryNames.canonical(name, language)
    if (canonical != name) Some(s"not-canonical (→ $canonical)")
    else Option.when(CountryNamesOtherLanguages(language.getLanguage).contains(name.toLowerCase(Locale.ROOT)))("in-another-language")
  }

  private val ServedLanguages = Seq(Locale.ENGLISH, Locale.GERMAN, Locale.of("es"), Locale.of("pl"))

  /** Per language: every country's name in the OTHER served languages that this language does not
   *  spell the same way (lower-cased). */
  private val CountryNamesOtherLanguages: Map[String, Set[String]] = {
    def namesIn(language: Locale): Set[String] = Locale.getISOCountries.toSet.flatMap { (iso: String) =>
      val name = Locale.of("", iso).getDisplayCountry(language)
      Option.when(name.nonEmpty && name != iso)(name.toLowerCase(Locale.ROOT))
    }
    val all = ServedLanguages.map(l => l.getLanguage -> namesIn(l)).toMap
    val own = all.map { case (code, names) =>
      code -> (names ++ (if (code == "pl") CountryNames.Polish.map(_.toLowerCase(Locale.ROOT)) else Set.empty))
    }
    all.keys.map(code => code -> (all.filter(_._1 != code).values.flatten.toSet -- own(code))).toMap
  }

  /** TMDB's genre names per served language. A genre is a taxonomy label, and TMDB's is the one a
   *  card normally carries; a cinema's own label passes so long as it is not ANOTHER language's
   *  TMDB label (an English "Comedy" on a Polish card) and not a list read as one label. */
  private val TmdbGenres: Map[String, Set[String]] = Map(
    "en" -> Set("Action", "Adventure", "Animation", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy",
      "History", "Horror", "Music", "Mystery", "Romance", "Science Fiction", "TV Movie", "Thriller", "War", "Western"),
    "pl" -> Set("Akcja", "Przygodowy", "Animacja", "Komedia", "Kryminał", "Dokumentalny", "Dramat", "Familijny", "Fantasy",
      "Historyczny", "Horror", "Muzyczny", "Tajemnica", "Romans", "Sci-Fi", "film TV", "Thriller", "Wojenny", "Western"),
    "de" -> Set("Action", "Abenteuer", "Animation", "Komödie", "Krimi", "Dokumentarfilm", "Drama", "Familie", "Fantasy",
      "Historie", "Horror", "Musik", "Mystery", "Liebesfilm", "Science Fiction", "TV-Film", "Thriller", "Kriegsfilm", "Western"),
    "es" -> Set("Acción", "Aventura", "Animación", "Comedia", "Crimen", "Documental", "Drama", "Familia", "Fantasía",
      "Historia", "Terror", "Música", "Misterio", "Romance", "Ciencia ficción", "Película de TV", "Suspense", "Bélica", "Western"))

  /** Another language's label a language uses as its own: Polish venues and Filmweb bill
   *  "Science Fiction" as readily as TMDB's "Sci-Fi", and German and English venues "Sci-Fi". */
  private val OwnLoans: Map[String, Set[String]] =
    Map("pl" -> Set("Science Fiction"), "de" -> Set("Sci-Fi"), "en" -> Set("Sci-Fi"))

  private val ListSeparator = """\s*[,/|;]\s*""".r

  private def genreProblem(language: Locale)(genre: String): Option[String] = {
    val code = language.getLanguage
    val own = TmdbGenres.getOrElse(code, Set.empty) ++ OwnLoans.getOrElse(code, Set.empty)
    val foreign = TmdbGenres.filter(_._1 != code).values.flatten.toSet -- own
    if (genre.trim.isEmpty) Some("empty")
    else if (ListSeparator.findFirstIn(genre).isDefined) Some("is-a-list")
    else Option.when(foreign.contains(genre))("in-another-language")
  }
}

/** The served cards that may break a rule, per country, each with why — and the verdict over one
 *  served corpus: what breaks a rule unexplained, and which entries the corpus serves that no
 *  longer break one (stale: the backlog only shrinks). An entry for a card the corpus does not
 *  serve says nothing — served corpora move with every recording. */
object ServedOutputAllowlist {

  private val FormatBeforeSuffix =
    "TODO(rule): a format tag followed by a strand or event suffix, so FormatTags' end-anchored strip misses it — the " +
      "same listings SearchQueryMarkersSpec parks (P9); a FormatTags peel before ' | <strand>' / ' – <event>' would fix all"
  private val CreditKeptInDisplay =
    "TODO(rule): 'reż. <director>' is stripped from the search (xtra-rez-credit-suffix, a GlobalStructural rule) but kept " +
      "in the display and the key; a Canonical-tier credit strip folds these rows, so it needs the five corpora's moves reviewed — parked"

  val entries: Map[Country, Map[AllowedCard, String]] = Map(
    Country.Poland -> Map(
      AllowedCard.anywhere("Tajemniczy świat Arrietty (napisy PL) | Poniedziałki ze studiem Ghibli") -> FormatBeforeSuffix,
      AllowedCard.anywhere("Chłopiec i czapla (napisy PL) | Poniedziałki ze Studiem Ghibli") -> FormatBeforeSuffix,
      AllowedCard.anywhere("Miloš Forman - Amadeusz 4K – pokaz specjalny") -> FormatBeforeSuffix,
      AllowedCard.anywhere("Fonomo 26 - Joybubbles reż. Rachel J. Morrison") -> CreditKeptInDisplay,
      AllowedCard.anywhere("Fonomo 26 - Trzy Pieśni reż. Krzysztof Nowicki") -> CreditKeptInDisplay,
      AllowedCard.anywhere("Fonomo 26 - Cała reszta to szum reż. Nicolas Pereda") -> CreditKeptInDisplay,
      AllowedCard.anywhere("Fonomo 26 - Nova '78 reż. Rodrigo Areias, Aaron Brookner") -> CreditKeptInDisplay,
      AllowedCard.anywhere("Weekend Seniora z Kulturą: Vivaldi i ja [2D Lektor] (seans z audiodeskrypcją)") -> FormatBeforeSuffix,
      AllowedCard.anywhere("Kusama. Nieskończoność (napisy Pl) | Tylko Sztuka Cię Nie Oszuka") -> FormatBeforeSuffix,
      AllowedCard("slawno", "Bez końca 2d pl lolo") ->
        "the venue's own billing ends '2d pl lolo' — a typo'd tail no rule should learn; it leaves with the listing"),
    Country.Germany -> Map(
      AllowedCard.anywhere("A Beautiful Planet - Ein IMAX 3D-Erlebnis") -> "the IMAX documentary's own German title (TMDB 400617)"))

  /** What a corpus's verdict may do to the build. A CHECKED-IN corpus (the Polish fixture boot) changes only
   *  with a commit, so a finding or a stale entry there is that commit's to answer: it fails. A RECORDED corpus
   *  moves nightly under every push, so a finding a fresh recording brings, or an entry it cures, is a DATA
   *  change, not the push's — it is reported, never failed (the convergence lane must not go red on a
   *  recording; cf. `CorpusShapeSpec.awaitingReRecord`). */
  sealed trait CorpusKind
  object CorpusKind {
    case object CheckedIn extends CorpusKind
    case object Recorded  extends CorpusKind
  }

  final case class Verdict(unexplained: Seq[String], stale: Seq[String]) {
    /** The lines that fail the build, and those only reported, for a corpus of `kind`. */
    def enforced(kind: CorpusKind): Enforced = {
      val lines = unexplained ++ stale.map(entry => s"allowlisted but no longer breaking a rule — drop: $entry")
      kind match {
        case CorpusKind.CheckedIn => Enforced(failures = lines, reported = Nil)
        case CorpusKind.Recorded  => Enforced(failures = Nil, reported = lines)
      }
    }
  }

  final case class Enforced(failures: Seq[String], reported: Seq[String])

  def judge(country: Country, normalizer: TitleNormalizer, cards: Seq[ServedCard]): Verdict = {
    val allowed = entries.getOrElse(country, Map.empty)
    def entryFor(city: String, title: String): Option[AllowedCard] =
      Seq(AllowedCard(city, title), AllowedCard.anywhere(title)).find(allowed.contains)
    val found = ServedOutputInvariants.violations(country, normalizer, cards)
    val unexplained = found.filter(f => entryFor(f.city, f.title).isEmpty)
      .map(f => s"${country.code} / ${f.city} / '${f.title}': ${f.finding}").distinct.sorted
    val served = cards.flatMap(c => Seq(AllowedCard(c.city.slug, c.title), AllowedCard.anywhere(c.title))).toSet
    val stale = ((allowed.keySet & served) -- found.flatMap(f => entryFor(f.city, f.title))).toSeq.map(_.toString).sorted
    Verdict(unexplained, stale)
  }
}
