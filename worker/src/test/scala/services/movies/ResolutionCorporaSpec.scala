package services.movies

import clients.TmdbClient
import models._
import org.scalacheck.{Gen, Shrink}
import org.scalactic.anyvals.PosInt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{ResolutionCache, TmdbBasis}
import tools.RoutingHttpFetch

/**
 * The resolver's half of the matching corpus: every film TMDB resolution has landed on
 * the WRONG entry of a same-titled, same-director or same-series pair, replayed through
 * `TmdbCandidateSearch.resolve` against a stubbed TMDB — plus two metamorphic
 * properties over those same scenarios:
 *
 *   - evidence that AGREES with the right film (another venue listing it, the film's own
 *     runtime and year, derived-slot noise) never moves the answer;
 *   - nothing a TitleOnly guess wrote onto the row (its key year, its Tmdb/Imdb/Filmweb
 *     slots) is ever read back as evidence for that guess.
 *
 * `MatchingCorporaSpec` (common) holds the canonicaliser's half.
 */
class ResolutionCorporaSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt.ensuringValid(60))
  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  /** Routes tried IN ORDER, first fragment contained in the URL wins; anything
   *  unrouted is an empty TMDB answer, so a scenario states only what it needs. */
  private def tmdbOf(routes: (String, String)*) =
    new TmdbClient(http = RoutingHttpFetch.getOnly(routes :+ ("" -> """{"results":[],"crew":[]}""")), apiKey = Some(settings.TmdbApiKey("stub")))

  private def search(tmdb: TmdbClient) =
    new TmdbCandidateSearch(tmdb, titleNormalizer, ResolutionCache.passthrough, letterboxdIdResolver = None, wikidata = None)

  private def hit(id: Int, title: String, date: String, original: String = "", popularity: Double = 1.0) =
    s"""{"id":$id,"title":"$title","original_title":"${if (original.isEmpty) title else original}","release_date":"$date","popularity":$popularity}"""

  private def results(hits: String*) = s"""{"results":[${hits.mkString(",")}]}"""

  private def person(id: Int, name: String) = s"""{"results":[{"id":$id,"name":"$name","known_for_department":"Directing"}]}"""

  private def credits(hits: String*) =
    s"""{"crew":[${hits.map(_.stripSuffix("}") + ""","department":"Directing","job":"Director"}""").mkString(",")}]}"""

  private def details(id: Int, title: String, date: String, runtime: Int, director: String) =
    s"""{"id":$id,"title":"$title","original_title":"$title","release_date":"$date","runtime":$runtime,"credits":{"crew":[{"job":"Director","name":"$director"}],"cast":[]}}"""

  /** One resolver scenario: a TMDB, a row as the venues publish it, and the one right answer. */
  private final case class Scenario(
    sha: String, what: String, tmdb: TmdbClient, title: String, year: Option[Int], row: MovieRecord,
    expected: Option[Int], venueSlot: SourceData
  )

  private def venues(slot: SourceData, cinemas: Cinema*): Map[Source, SourceData] =
    cinemas.map(c => (c: Source) -> slot).toMap

  // ── The scenarios ─────────────────────────────────────────────────────────

  /** 69a4916f5: TMDB's pl-PL search for "Lalka" at 2026 returns TWO exact titles —
   *  Kawalski's real film and a French film TMDB also calls "Lalka". Popularity
   *  picked the French one for dozens of venues. Two exact matches is ambiguity. */
  private val lalka = {
    val slot = SourceData(title = Some("Lalka"), releaseYear = Some(2026))
    Scenario("69a4916f5", "Lalka: two exact 2026 titles refuse rather than guess by popularity",
      tmdbOf("/search/movie" -> results(
        hit(1309396, "Lalka", "2026-02-01", original = "La Poupée", popularity = 9.0),
        hit(1321666, "Lalka", "2026-10-01", popularity = 2.5))),
      "Lalka", Some(2026), MovieRecord(data = venues(slot, Helios)), expected = None, slot)
  }

  /** e3d6c671b: Jan Sobierajski made "Mistyczka" and "Maryja. Matka Papieża",
   *  both 2026. The row's derived Filmweb slot named the other film, and lowest-id
   *  handed it the row every cycle. The cinemas' title outranks a derived one. */
  private val mistyczka = {
    val slot = SourceData(title = Some("Mistyczka"), releaseYear = Some(2026), director = Seq("Jan Sobierajski"))
    Scenario("e3d6c671b", "Mistyczka: the cinemas' title beats the derived slot's same-director film",
      tmdbOf(
        "query=Jan+Sobierajski"       -> person(7100, "Jan Sobierajski"),
        "/person/7100/movie_credits"  -> credits(hit(1646379, "Maryja. Matka Papieża", "2026-03-01"), hit(1731866, "Mistyczka", "2026-05-01")),
        "/movie/1646379?"             -> details(1646379, "Maryja. Matka Papieża", "2026-03-01", 62, "Jan Sobierajski"),
        "/movie/1731866?"             -> details(1731866, "Mistyczka", "2026-05-01", 87, "Jan Sobierajski")),
      "Mistyczka", Some(2026),
      MovieRecord(data = venues(slot, KinoMuza, Helios, Multikino) +
        ((Filmweb: Source) -> SourceData(title = Some("Mistyczka"), originalTitle = Some("Maryja. Matka Papieża")))),
      expected = Some(1731866), slot)
  }

  /** 8725f35ae: once the wrong merge had happened, a venue named EACH film as a cinema
   *  title — three for "Mistyczka", Kino Klaps's lone "Maryja. Matka Papieża" — so both
   *  credits sat in the cinema-exact tier and lowest id (1646379) took the row. The credit
   *  the most venues name wins; lowest id only breaks a real tie. */
  private val mistyczkaMerged = {
    val slot = SourceData(title = Some("Mistyczka"), releaseYear = Some(2026), director = Seq("Jan Sobierajski"))
    mistyczka.copy(sha = "8725f35ae", what = "Mistyczka: the film most venues name beats a lone venue's other film",
      row = MovieRecord(data = venues(slot, KinoMuza, Helios, Multikino) +
        ((KinoKlaps: Source) -> slot.copy(title = Some("Maryja. Matka Papieża")))))
  }

  /** 544ed41e9: Kino Malta credits Michel Franco; the row's stale Tmdb slot carried Dag
   *  Johan Haugerud's "Drømmer" and his name, which sorted first and re-won. */
  private val dreams = {
    val slot = SourceData(title = Some("Dreams"), releaseYear = Some(2025), director = Seq("Michel Franco"))
    Scenario("544ed41e9", "Dreams: a derived slot's director never hints the walk",
      tmdbOf(
        "query=Michel+Franco"           -> person(5000, "Michel Franco"),
        "query=Dag+Johan+Haugerud"      -> person(5001, "Dag Johan Haugerud"),
        "/person/5000/movie_credits"    -> credits(hit(1134463, "Dreams", "2025-07-10", original = "Dreams: Sueños")),
        "/person/5001/movie_credits"    -> credits(hit(1228682, "Dreams", "2024-10-01", original = "Drømmer")),
        "/movie/1134463?"               -> details(1134463, "Dreams", "2025-07-10", 98, "Michel Franco"),
        "/movie/1228682?"               -> details(1228682, "Dreams", "2024-10-01", 110, "Dag Johan Haugerud")),
      "Dreams", Some(2025),
      MovieRecord(tmdbId = Some(1228682), data = venues(slot, Helios) +
        ((Tmdb: Source) -> SourceData(title = Some("Dreams"), originalTitle = Some("Drømmer"),
          director = Seq("Dag Johan Haugerud"), releaseYear = Some(2024)))),
      expected = Some(1134463), slot)
  }

  /** f430c1de5: a venue's typo ("Mockinjay") put both parts inside `TitleMatch.close`,
   *  and the lowest-id tie-break handed Part 2's row to Part 1. */
  private val mockingjay = {
    val slot = SourceData(title = Some("The Hunger Games: Mockinjay - Part 2"), releaseYear = Some(2015),
      director = Seq("Francis Lawrence"))
    Scenario("f430c1de5", "Mockingjay: a typo'd Part 2 is not Part 1",
      tmdbOf(
        "query=Francis+Lawrence"      -> person(6000, "Francis Lawrence"),
        "/person/6000/movie_credits"  -> credits(
          hit(131631, "The Hunger Games: Mockingjay - Part 1", "2014-11-19"),
          hit(131634, "The Hunger Games: Mockingjay - Part 2", "2015-11-18")),
        "/movie/131631?"              -> details(131631, "The Hunger Games: Mockingjay - Part 1", "2014-11-19", 123, "Francis Lawrence"),
        "/movie/131634?"              -> details(131634, "The Hunger Games: Mockingjay - Part 2", "2015-11-18", 137, "Francis Lawrence")),
      "The Hunger Games: Mockinjay - Part 2", Some(2015), MovieRecord(data = venues(slot, Helios)),
      expected = Some(131634), slot)
  }

  /** Found by `MatchingPropertySpec`: the same fuzzy-match collision as Mockingjay, reached
   *  through a rerelease year a venue stamps on the title (the Flicks/Odeon shape of
   *  2948a4041). "Kill Bil: Vol. 2 (2026)" and "Kill Bill: Vol. 1" run different lengths,
   *  so the equal-length instalment check never compared their numbers. */
  private val killBill = {
    val slot = SourceData(title = Some("Kill Bil: Vol. 2 (2026)"), releaseYear = Some(2004),
      director = Seq("Quentin Tarantino"))
    Scenario("new", "Kill Bill: a rerelease-stamped, typo'd Vol. 2 is not Vol. 1",
      tmdbOf(
        "query=Quentin+Tarantino"     -> person(138, "Quentin Tarantino"),
        "/person/138/movie_credits"   -> credits(hit(24, "Kill Bill: Vol. 1", "2003-10-10"), hit(393, "Kill Bill: Vol. 2", "2004-04-16")),
        "/movie/24?"                  -> details(24, "Kill Bill: Vol. 1", "2003-10-10", 111, "Quentin Tarantino"),
        "/movie/393?"                 -> details(393, "Kill Bill: Vol. 2", "2004-04-16", 136, "Quentin Tarantino")),
      "Kill Bil: Vol. 2 (2026)", Some(2004), MovieRecord(data = venues(slot, Helios)),
      expected = Some(393), slot)
  }

  /** 98bf5142d: a bare-title search picked a 9-minute 1960 short for "Homo sapiens?";
   *  the key was re-stamped 1960, and every later search at 1960 found the short again
   *  while twelve venues published 2025. */
  private val HomoSapiensShort   = 290001
  private val HomoSapiensFeature = 1400002
  private val homoSapiensTmdb = tmdbOf(
    "year=1960"     -> results(hit(HomoSapiensShort, "Homo sapiens?", "1960-01-01")),
    "year=2025"     -> results(hit(HomoSapiensFeature, "Homo sapiens?", "2025-09-01")),
    "/search/movie" -> results(hit(HomoSapiensShort, "Homo sapiens?", "1960-01-01"), hit(HomoSapiensFeature, "Homo sapiens?", "2025-09-01")),
    s"/movie/$HomoSapiensShort?"   -> details(HomoSapiensShort, "Homo sapiens?", "1960-01-01", 9, "Ion Popescu-Gopo"),
    s"/movie/$HomoSapiensFeature?" -> details(HomoSapiensFeature, "Homo sapiens?", "2025-09-01", 95, "Nikolaus Geyrhalter"))
  private val homoSapiensVenue = SourceData(title = Some("Homo sapiens?"), releaseYear = Some(2025))
  private val homoSapiens = Scenario("98bf5142d", "Homo sapiens?: a TitleOnly guess's key year is not evidence",
    homoSapiensTmdb, "Homo sapiens?", Some(1960),
    MovieRecord(tmdbId = Some(HomoSapiensShort), tmdbBasis = Some(TmdbBasis.TitleOnly.toString),
      data = venues(homoSapiensVenue, Helios, Multikino) +
        ((Tmdb: Source) -> SourceData(title = Some("Homo sapiens?"), releaseYear = Some(1960), runtimeMinutes = Some(9)))),
    expected = Some(HomoSapiensFeature), homoSapiensVenue)

  private val scenarios = Seq(lalka, mistyczka, mistyczkaMerged, dreams, mockingjay, killBill, homoSapiens)

  private def resolve(s: Scenario, row: MovieRecord, year: Option[Int]): Option[Int] =
    search(s.tmdb).resolve(s.title, year, row).map(_._1)

  "the resolver corpus" should "resolve every historical scenario to the right film, or refuse where TMDB cannot tell" in {
    for (s <- scenarios) withClue(s"[${s.sha}] ${s.what}: ") {
      resolve(s, s.row, s.year) shouldBe s.expected
    }
  }

  // ── Properties ────────────────────────────────────────────────────────────

  private val spareVenues: Seq[Cinema] = Seq(KinoApollo, KinoMuza, Kinoteka, KinoPodBaranami, KinoMikro, KinoPalacowe)

  /** 1–3 more venues publishing what the right film's venues already publish, under a
   *  decoration the title rules recognise, sometimes with the film's own runtime. */
  private def genCorroboration(s: Scenario): Gen[Map[Source, SourceData]] = for {
    n        <- Gen.choose(1, 3)
    cinemas  <- Gen.pick(n, spareVenues.filterNot(c => s.row.data.contains(c)))
    decorate <- Gen.listOfN(n, Gen.oneOf[String => String](identity, t => s"$t 2D", t => s"$t (napisy)", t => s"Kino seniora: $t"))
  } yield cinemas.zip(decorate).map { case (c, d) =>
    (c: Source) -> s.venueSlot.copy(title = s.venueSlot.title.map(d))
  }.toMap

  "evidence that agrees with the right film" should "never move a scenario's resolution" in {
    forAll(Gen.oneOf(scenarios).flatMap(s => genCorroboration(s).map(s -> _))) { case (s, extra) =>
      withClue(s"[${s.sha}] ${s.what} + ${extra.map { case (k, v) => k -> v.title }}: ") {
        resolve(s, s.row.copy(data = s.row.data ++ extra), s.year) shouldBe s.expected
      }
    }
  }

  /** Whatever a TitleOnly guess could have written back onto the row: the key year it
   *  re-stamped, and derived slots carrying the guessed film's year, runtime and credit. */
  private val genGuessEcho: Gen[(Option[Int], Map[Source, SourceData])] = for {
    keyYear <- Gen.oneOf(Some(1960), Some(1961), None)
    sources <- Gen.someOf(Seq[Source](Tmdb, Imdb, Filmweb))
    year    <- Gen.oneOf(1959, 1960, 1961)
    runtime <- Gen.oneOf(9, 10)
  } yield keyYear -> sources.map(src => src -> SourceData(title = Some("Homo sapiens?"), releaseYear = Some(year),
    runtimeMinutes = Some(runtime), director = Seq("Ion Popescu-Gopo"))).toMap

  "a value derived from a TitleOnly resolution" should "never be read back as evidence for that resolution" in {
    forAll(genGuessEcho) { case (keyYear, derived) =>
      val row = homoSapiens.row.copy(data = homoSapiens.row.data.filter { case (src, _) => Source.cinemaOf(src).isDefined } ++ derived)
      withClue(s"keyYear=$keyYear derived=${derived.keySet}: ") {
        resolve(homoSapiens, row, keyYear) shouldBe Some(HomoSapiensFeature)
      }
    }
  }
}
