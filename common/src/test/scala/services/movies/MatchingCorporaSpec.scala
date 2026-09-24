package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Every pair of films the pipeline has wrongly merged, split or mis-resolved onto
 * each other, replayed through the real decision functions — `groupByFilm` +
 * `clusterByFilm` for rows, `SequelMarker` / `TitleContainment` / `sanitize` /
 * `searchKey` for bare titles.
 *
 * Twenty-odd fixes between 2026-08 and 2026-09 each pinned ONE pair next to the rule
 * it broke; five films were wrong in prod for weeks before any of them. This file is
 * the regression corpus as one list, so a change to any rule is measured against
 * every past incident at once, not only the one its author was thinking of. Each
 * entry names the commit whose fix it pins. Resolver-level pairs (Lalka, Mistyczka's
 * director walk, Mockingjay's fuzzy match, Homo sapiens) live in the worker's
 * `ResolutionCorporaSpec`, where the TMDB stub is.
 *
 * Add a pair whenever a merge/split/mis-resolve is fixed.
 */
class MatchingCorporaSpec extends AnyFlatSpec with Matchers {

  import CanonicalizerRows._

  private final case class Case(sha: String, what: String, rows: Seq[Row], a: CacheKey, b: CacheKey)

  private def pair(sha: String, what: String)(a: Row, b: Row, context: Row*): Case =
    Case(sha, what, Seq(a, b) ++ context, a._1, b._1)

  /** A row whose only cinema slot published a title and nothing else — the shape of a
   *  small chain's listing, and of every UK rerelease-season page. */
  private def bare(title: String, year: Option[Int], cinema: Source, tmdbId: Option[Int] = None): Row =
    cacheKey(title, year) -> MovieRecord(
      tmdbId = tmdbId,
      data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), releaseYear = year)) ++
        tmdbId.map(_ => (Tmdb: Source) -> SourceData(releaseYear = year)))

  // ── NEVER merge ────────────────────────────────────────────────────────────

  private val neverMerge: Seq[Case] = Seq(
    // The containment edge: an unresolved sequel folded onto its resolved first film.
    pair("130b89c55", "a re-released Mockingjay Pt 2 is not the 2012 original")(
      resolved("The Hunger Games", 70160, 2012, Helios),
      unresolved("The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)", None, Multikino)),
    pair("130b89c55", "Toy Story 5 is not Toy Story")(
      resolved("Toy Story", 862, 1995, Helios), unresolved("Toy Story 5", None, Multikino)),
    pair("130b89c55", "Rocky II is not Rocky")(
      resolved("Rocky", 1366, 1976, Helios), unresolved("Rocky II", None, Multikino)),
    pair("130b89c55", "Blade Runner 2049 is not Blade Runner")(
      resolved("Blade Runner", 78, 1982, Helios), unresolved("Blade Runner 2049", None, Multikino)),
    pair("130b89c55", "a numbered Polish part is not the first film")(
      resolved("Szybcy i wściekli", 9799, 2001, Helios), unresolved("Szybcy i wściekli: część 8", None, Multikino)),
    pair("d6f65d7d7", "a spelled-out part is not the first film")(
      resolved("Diuna", 438631, 2021, Helios), unresolved("Diuna: Część druga", None, Multikino)),
    pair("f5d7f00b3", "Catching Fire (no ordinal) is not the original")(
      resolved("The Hunger Games", 70160, 2012, Helios), unresolved("The Hunger Games: Catching Fire", None, Multikino)),
    pair("60820c6c6", "a renamed franchise entry is not the original")(
      resolved("Bring It On", 10110, 2000, Helios), unresolved("Bring It On: All or Nothing", None, Multikino)),
    // Rule 1: a shared (wrong) tmdbId is not permission to merge.
    pair("15bd9627b", "Mistyczka and Maryja. Matka Papieża under one tmdbId")(
      published("Mistyczka", 1646379, 2026, KinoMuza, "Mistyczka", 87),
      published("DOBRE Kino - Maryja. Matka Papieża", 1646379, 2026, KinoMuzeumGdansk, "Maryja. Matka Papieża", 62)),
    pair("476a20791", "two curated Hunger Games siblings under one wrong tmdbId")(
      bare("The Hunger Games: Catching Fire", Some(2026), Helios, Some(1300968)),
      bare("The Hunger Games: The Ballad of Songbirds and Snakes", Some(2026), Multikino, Some(1300968))),
    pair("476a20791", "Mockingjay Part 1 and Part 2 under one wrong tmdbId")(
      bare("The Hunger Games: Mockingjay - Part 1", Some(2026), Helios, Some(1300968)),
      bare("The Hunger Games: Mockingjay - Part 2", Some(2026), Multikino, Some(1300968))),
    pair("2948a4041", "a rerelease-stamped Part 1 is still not Part 2")(
      bare("The Hunger Games: Mockingjay - Part 1 (2026)", Some(2026), Helios, Some(1300968)),
      bare("The Hunger Games: Mockingjay - Part 2", Some(2026), Multikino, Some(1300968))),
    // Two resolved same-titled films, and a bare listing that could be either.
    pair("c18f2be24", "Zaproszenie 1986 is not Zaproszenie 2026")(
      resolved("Zaproszenie", 110410, 1986, Helios), resolved("Zaproszenie", 950028, 2026, Multikino)),
    pair("c18f2be24", "a bare Zaproszenie does not guess the 1986 film")(
      unresolved("Zaproszenie", None, Kinoteka), resolved("Zaproszenie", 110410, 1986, Helios),
      resolved("Zaproszenie", 950028, 2026, Multikino)),
    pair("c18f2be24", "a bare Zaproszenie does not guess the 2026 film")(
      unresolved("Zaproszenie", None, Kinoteka), resolved("Zaproszenie", 950028, 2026, Multikino),
      resolved("Zaproszenie", 110410, 1986, Helios)),
    pair("a1b228643", "Scarface 1932 is not Scarface 1983")(
      resolved("Scarface", 877, 1932, Helios), resolved("Scarface", 111, 1983, Multikino)),
    pair("d8ab121b2", "a bare Diuna does not guess between two unresolved years")(
      unresolved("Diuna", None, Kinoteka), unresolved("Diuna", Some(1984), Helios),
      unresolved("Diuna", Some(2021), Multikino)),
    // Rule 4: a yearless row whose own evidence contradicts the one resolved film.
    pair("437d1fa21", "the 2014 Hope is not the 2026 Hope")(
      resolved("Hope", 1058424, 2026, Helios),
      cacheKey("Hope", None) -> MovieRecord(data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Hope"), releaseYear = Some(2014), runtimeMinutes = Some(91))))),
    pair("437d1fa21", "I Was A Stranger is not L'étranger")(
      published("Obcy", 7183, 2025, Helios, "L'étranger", 120),
      cacheKey("Obcy", None) -> MovieRecord(data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Obcy"), originalTitle = Some("I Was A Stranger"), runtimeMinutes = Some(103))))),
    pair("0fe64f423", "Ktoś całkiem obcy is not a decoration of Obcy")(
      published("Obcy", 7183, 2025, Helios, "L'étranger", 122),
      cacheKey("Ktoś całkiem obcy", Some(2024)) -> MovieRecord(data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Ktoś całkiem obcy"), releaseYear = Some(2024),
          originalTitle = Some("I Was A Stranger"), runtimeMinutes = Some(103))))),
    pair("e5f102dd1", "It (1990) is not the 2017 It")(
      resolved("It", 346364, 2017, Helios), unresolved("It (1990)", Some(1990), Multikino))
  )

  // ── ALWAYS merge ───────────────────────────────────────────────────────────

  private val alwaysMerge: Seq[Case] = Seq(
    pair("130b89c55", "a toddler-club banner decorates Toy Story 5")(
      resolved("Toy Story 5", 1084244, 2026, Helios), unresolved("Toddler Club: Toy Story 5", None, Multikino)),
    pair("130b89c55", "a chain's anniversary banner decorates The Matrix")(
      resolved("The Matrix", 603, 1999, Helios), unresolved("Cineworld 30: The Matrix", None, Multikino)),
    pair("130b89c55", "a year is never an ordinal")(
      resolved("Casablanca", 289, 1942, Helios), unresolved("Casablanca 1942", None, Multikino)),
    pair("130b89c55", "a preview banner decorates Ojczyzna")(
      resolved("Ojczyzna", 1180000, 2026, Helios), unresolved("Ojczyzna - pokaz przedpremierowy", None, Multikino)),
    pair("130b89c55", "a re-release suffix decorates Top Gun")(
      resolved("Top Gun", 744, 1986, Helios), unresolved("Top Gun - Re-Release", None, Multikino)),
    pair("6c37ed48b", "a play-screening prefix decorates the play")(
      resolved("Fallen Angels by Noël Coward", 1500000, 2026, Helios),
      unresolved("gb Fallen Angels by Noël Coward.", None, Multikino)),
    pair("96d36af4e", "a senior-club banner decorates 500 Mil")(
      resolved("500 Mil", 1400000, 2026, Helios), unresolved("Kino Seniora - 500 Mil", None, Multikino)),
    pair("fd84f1fb1", "Filmowy Klub Seniora decorates its film")(
      resolved("Robin hood. Koniec legendy", 1300000, 2026, Helios),
      unresolved("Filmowy Klub Seniora i Seniorki: Robin hood. Koniec legendy", None, Multikino)),
    pair("3e9514172", "a retrospective banner decorates its film")(
      resolved("Brzezina", 42000, 1970, Helios), unresolved("WAJDA re-wizje: Brzezina", None, Multikino)),
    pair("apiQuery", "an accessibility programme decorates Freak Show")(
      resolved("Freak Show", 1200000, 2026, Helios), unresolved("Kino bez barier: Freak Show (AD + CC + PJM)", None, Multikino)),
    pair("apiQuery", "a dub edition is the film")(
      resolved("Straszny film", 4247, 2000, Helios), unresolved("Straszny film ukraiński dubbing", None, Multikino)),
    pair("xtra-canonical", "an anniversary suffix is the film")(
      resolved("Terminator 2: Dzień sądu", 280, 1991, Helios), unresolved("Terminator 2: Dzień sądu - 35. Rocznica", Some(1991), Multikino)),
    pair("2bd0d2900", "& and i spell one film")(
      resolved("Mandalorian i Grogu", 1022789, 2026, Helios), unresolved("Mandalorian & Grogu", Some(2026), Multikino)),
    pair("canonical", "the Gwiezdne Wojny prefix is the film")(
      resolved("Mandalorian i Grogu", 1022789, 2026, Helios), unresolved("Gwiezdne Wojny: Mandalorian i Grogu", Some(2026), Multikino)),
    pair("e3699d05e", "Arabic and Roman ordinals spell one film")(
      resolved("Mortal Kombat II", 931285, 2026, Helios), unresolved("Mortal Kombat 2", Some(2026), Multikino)),
    pair("2948a4041", "a rerelease-stamped Part 1 is Part 1")(
      bare("The Hunger Games: Mockingjay - Part 1 (2026)", Some(2026), Helios, Some(131631)),
      bare("The Hunger Games: Mockingjay - Part 1", Some(2026), Multikino, Some(131631))),
    pair("476a20791", "an uncurated numbered suffix under one tmdbId is one film")(
      bare("Ghost 2", Some(2025), Helios, Some(700)), bare("Ghost 2 (1)", Some(2025), Multikino, Some(700))),
    pair("15bd9627b", "a translation under one tmdbId is one film")(
      resolved("Zaplątani", 38757, 2010, KinoMuza), resolved("Tangled", 38757, 2010, KinoMuzeumGdansk)),
    pair("437d1fa21", "a production-year straggler folds onto its resolved film")(
      resolved("Głos Hind Rajab", 1400001, 2025, Helios),
      cacheKey("Głos Hind Rajab", None) -> MovieRecord(data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Głos Hind Rajab"), releaseYear = Some(2022)))))
  )

  private def describe(c: Case) = s"[${c.sha}] ${c.what}: '${c.a.cleanTitle}' ${c.a.year} vs '${c.b.cleanTitle}' ${c.b.year}"

  private def partition(c: Case) = films(c.rows).map(_.map(r => (r._1.cleanTitle, r._1.year)))

  "the never-merge corpus" should "keep every historical pair in separate films, in either row order" in {
    for (c <- neverMerge; rows <- Seq(c.rows, c.rows.reverse)) withClue(s"${describe(c)}\nfilms: ${partition(c)}\n") {
      sameFilm(rows, c.a, c.b) shouldBe false
    }
  }

  "the always-merge corpus" should "put every historical pair in one film, in either row order" in {
    for (c <- alwaysMerge; rows <- Seq(c.rows, c.rows.reverse)) withClue(s"${describe(c)}\nfilms: ${partition(c)}\n") {
      sameFilm(rows, c.a, c.b) shouldBe true
    }
  }

  // ── Bare titles ────────────────────────────────────────────────────────────

  /** Two titles naming different instalments of one series — the title ALONE must
   *  say so, because the cinemas' own evidence is usually silent (UK slots publish an
   *  original title one time in nine). */
  private val differentInstalments: Seq[(String, String, String)] = Seq(
    ("f430c1de5", "The Hunger Games: Mockingjay - Part 1", "The Hunger Games: Mockingjay - Part 2"),
    ("f430c1de5", "The Hunger Games: Mockinjay - Part 1", "The Hunger Games: Mockingjay - Part 2"),
    ("f430c1de5", "Rocky II", "Rocky III"),
    ("f430c1de5", "Kingsman 2", "Kingsman 3"),
    ("f430c1de5", "Toy Story", "Toy Story 5"),
    ("130b89c55", "The Hunger Games", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)"),
    ("130b89c55", "Star Wars", "Star Wars: Episode IV"),
    ("130b89c55", "Szybcy i wściekli", "Szybcy i wściekli: część 8"),
    ("d6f65d7d7", "Dune", "Dune: Part Two"),
    ("d6f65d7d7", "Wicked", "Wicked: Part Three"),
    ("260bc21e3", "The Hunger Games: Catching Fire", "The Hunger Games: Mockingjay - Part 1"),
    ("260bc21e3", "The Hunger Games: The Ballad of Songbirds and Snakes", "The Hunger Games: Mockingjay - Part 2"),
    ("8176372e4", "The Hunger Games: Catching Fire", "The Hunger Games: Sunrise on the Reaping"),
    ("2948a4041", "The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 2"),
    ("60820c6c6", "Bring It On", "Bring It On: All or Nothing")
  )

  /** Two spellings of ONE film that the instalment check must not tell apart. */
  private val sameInstalment: Seq[(String, String, String)] = Seq(
    ("e3699d05e", "Mortal Kombat 2", "Mortal Kombat II"),
    ("e3699d05e", "Dune: Part 2", "Dune: Part Two"),
    ("2948a4041", "The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 1"),
    ("2948a4041", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)", "The Hunger Games: Mockingjay - Part 2"),
    ("2948a4041", "The Hunger Games: The Ballad of Songbirds & Snakes", "The Hunger Games: The Ballad of Songbirds and Snakes"),
    ("f430c1de5", "Guru", "Gourou"),
    ("130b89c55", "Toy Story 5", "Toddler Club: Toy Story 5"),
    ("130b89c55", "Casablanca", "Casablanca 1942"),
    ("130b89c55", "The Matrix", "Cineworld 30: The Matrix")
  )

  private def tokens(t: String) = TitleContainment.tokens(t)

  "SequelMarker.differentInstalments" should "tell every historical pair of instalments apart, both ways round" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe true
      SequelMarker.differentInstalments(tokens(b), tokens(a)) shouldBe true
    }
  }

  it should "never tell two spellings of one instalment apart" in {
    for ((sha, a, b) <- sameInstalment) withClue(s"[$sha] '$a' vs '$b': ") {
      SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe false
      SequelMarker.differentInstalments(tokens(b), tokens(a)) shouldBe false
    }
  }

  "TitleContainment.decorates" should "never read one instalment as a decoration of another" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      TitleContainment.decorates(tokens(a), tokens(b)) shouldBe false
      TitleContainment.decorates(tokens(b), tokens(a)) shouldBe false
    }
  }

  "the title keys" should "never give two instalments one merge key or one search key" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      titleNormalizer.sanitize(a) should not be titleNormalizer.sanitize(b)
      FilmCanonicalizer.searchKey(a, titleNormalizer) should not be FilmCanonicalizer.searchKey(b, titleNormalizer)
    }
  }
}
