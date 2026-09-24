package services.movies

import models._
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators.{genFilmEvidence, genCandidate}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{FilmEvidence, Verdict}

/**
 * The matching rules as METAMORPHIC properties: transform a title or a piece of
 * evidence in a way that cannot change which film it names (or, for an instalment,
 * in a way that always does), and the decision must move — or stay — accordingly.
 *
 * `MatchingCorporaSpec` replays the pairs that went wrong; this generates the pairs
 * that have not gone wrong YET. Every property is asked of the real decision
 * functions (`SequelMarker`, `TitleContainment`, `sanitize`, `searchKey`,
 * `groupByFilm` + `clusterByFilm`, `SamePerson`, `Verdict`), so it runs at unit speed.
 */
class MatchingPropertySpec extends IdentityPropertySpec {

  import CanonicalizerRows._

  private def tokens(t: String) = TitleContainment.tokens(t)

  // ── Instalments ────────────────────────────────────────────────────────────

  /** Series titles in the catalogue's languages, none ending in a numeral of its own. */
  private val seriesBases: Seq[String] = Seq(
    "Toy Story", "Rocky", "Diuna", "Kingsman", "Szybcy i wściekli", "Obcy",
    "The Hunger Games: Mockingjay", "Mortal Kombat", "Minionki", "Zaplątani")

  private val Roman = Map(2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V", 6 -> "VI", 7 -> "VII", 8 -> "VIII", 9 -> "IX")
  private val EnglishWords = Map(2 -> "Two", 3 -> "Three", 4 -> "Four", 5 -> "Five")
  private val PolishWords  = Map(2 -> "druga", 3 -> "trzecia", 4 -> "czwarta", 5 -> "piąta")

  /** Every way a venue or TMDB numbers instalment `n` of `base`. */
  private def notations(base: String, n: Int): Seq[String] =
    Seq(s"$base $n", s"$base: Part $n", s"$base - Part $n", s"$base Pt $n", s"$base: Część $n",
        s"$base: Chapter $n", s"$base Vol. $n") ++
      Roman.get(n).map(r => s"$base $r") ++
      EnglishWords.get(n).map(w => s"$base: Part $w") ++
      PolishWords.get(n).map(w => s"$base: Część $w")

  private val genInstalments: Gen[(String, Int, Int)] = for {
    base <- Gen.oneOf(seriesBases)
    n    <- Gen.choose(2, 9)
    m    <- Gen.choose(2, 9).suchThat(_ != n)
  } yield (base, n, m)

  private def genNotation(base: String, n: Int): Gen[String] = Gen.oneOf(notations(base, n))

  "titles differing only in an instalment ordinal" should "always read as different instalments, in any notation" in {
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) }) {
      case (a, b) =>
        SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe true
        SequelMarker.differentInstalments(tokens(b), tokens(a)) shouldBe true
    }
  }

  it should "never decorate one another, nor share a merge or search key" in {
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) }) {
      case (a, b) =>
        TitleContainment.decorates(tokens(a), tokens(b)) shouldBe false
        TitleContainment.decorates(tokens(b), tokens(a)) shouldBe false
        titleNormalizer.sanitize(a) should not be titleNormalizer.sanitize(b)
        FilmCanonicalizer.searchKey(a, titleNormalizer) should not be FilmCanonicalizer.searchKey(b, titleNormalizer)
    }
  }

  it should "never cluster, whichever side is resolved and whether the other carries a year" in {
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) },
           Gen.option(Gen.oneOf(2025, 2026))) { case ((a, b), year) =>
      val rows = Seq(resolved(a, 1, 2026, Helios), unresolved(b, year, Multikino))
      withClue(s"films: ${films(rows).map(_.map(_._1))}\n") {
        sameFilm(rows, rows(0)._1, rows(1)._1) shouldBe false
      }
    }
  }

  it should "stay different instalments when either side carries a rerelease year or a format tag" in {
    val trailing = Seq(" (2026)", " (2026 Re-Release)", " 4K", " 3D", " - Re-Release")
    forAll(genInstalments.flatMap { case (base, n, m) => genNotation(base, n).flatMap(a => genNotation(base, m).map(a -> _)) },
           Gen.oneOf(trailing)) { case ((a, b), tag) =>
      SequelMarker.differentInstalments(tokens(a + tag), tokens(b)) shouldBe true
      SequelMarker.differentInstalments(tokens(b), tokens(a + tag)) shouldBe true
    }
  }

  "an unnumbered title" should "never be the same film as its numbered sequel" in {
    forAll(Gen.oneOf(seriesBases).flatMap(base => Gen.choose(2, 9).flatMap(n => genNotation(base, n).map(base -> _)))) {
      case (base, sequel) =>
        SequelMarker.differentInstalments(tokens(base), tokens(sequel)) shouldBe true
        TitleContainment.decorates(tokens(base), tokens(sequel)) shouldBe false
        val rows = Seq(resolved(base, 1, 2020, Helios), unresolved(sequel, None, Multikino))
        sameFilm(rows, rows(0)._1, rows(1)._1) shouldBe false
    }
  }

  "two notations of ONE instalment" should "never read as different instalments" in {
    forAll(Gen.oneOf(seriesBases), Gen.choose(2, 9)) { (base, n) =>
      val all = notations(base, n)
      for (a <- all; b <- all) withClue(s"'$a' vs '$b': ") {
        SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe false
      }
    }
  }

  // ── Programme decoration ──────────────────────────────────────────────────

  /** Films a venue decorates, including a numbered instalment (a decoration must not
   *  read as — or hide — an ordinal). */
  private val decoratedBases: Seq[String] = Seq(
    "Diuna", "Ojczyzna", "Freak Show", "Terminator 2: Dzień sądu", "Mandalorian i Grogu",
    "Toy Story 5", "Mortal Kombat II", "Straszny film", "Chłopi", "Zawieście czerwone latarnie")

  /** Decorations the title rules RECOGNISE: the search key strips them, so the resolver
   *  queries the base film and the settle's search-title edge unions the rows. */
  private val recognisedDecorations: Seq[String => String] = Seq(
    b => s"Kino seniora: $b", b => s"Filmowy Klub Seniora: $b", b => s"Kino bez barier: $b (AD + CC + PJM)",
    b => s"$b (2026)", b => s"$b - pokaz przedpremierowy", b => s"$b – przedpremiera", b => s"$b - premiera",
    b => s"$b 4K", b => s"$b 2D", b => s"$b 3D", b => s"$b (napisy)", b => s"$b (dubbing)",
    b => s"$b ukraiński dubbing", b => s"$b - wersja oryginalna", b => s"$b - 35. Rocznica",
    b => s"DKF: $b", b => s"Poranek dla dzieci: $b", b => s"WAJDA: re-wizje: $b", b => s"$b | Kinoteka dla rodziców",
    b => b.toUpperCase(java.util.Locale.ROOT), b => s"  $b  ")

  /** Banners NO rule knows. The containment edge folds them onto a RESOLVED base; the
   *  other direction (a bare listing onto a resolved banner) is not a decoration. */
  private val unrecognisedBanners: Seq[String => String] = Seq(
    b => s"Toddler Club: $b", b => s"$b (wersja reżyserska)", b => s"$b [2D napisy]", b => s"Cineworld 30: $b")

  "a recognised programme decoration" should "never change the search key the resolver queries by" in {
    forAll(Gen.oneOf(decoratedBases), Gen.oneOf(recognisedDecorations)) { (base, decorate) =>
      FilmCanonicalizer.searchKey(decorate(base), titleNormalizer) shouldBe FilmCanonicalizer.searchKey(base, titleNormalizer)
    }
  }

  it should "fold onto its film in either direction: added to the unresolved row or removed from the resolved one" in {
    forAll(Gen.oneOf(decoratedBases), Gen.oneOf(recognisedDecorations), Gen.option(Gen.const(2026))) { (base, decorate, year) =>
      val added   = Seq(resolved(base, 1, 2026, Helios), unresolved(decorate(base), year, Multikino))
      val removed = Seq(resolved(decorate(base), 1, 2026, Helios), unresolved(base, year, Multikino))
      for (rows <- Seq(added, removed)) withClue(s"films: ${films(rows).map(_.map(_._1))}\n") {
        sameFilm(rows, rows(0)._1, rows(1)._1) shouldBe true
      }
    }
  }

  "an unrecognised banner" should "fold onto its resolved film through the containment edge" in {
    forAll(Gen.oneOf(decoratedBases), Gen.oneOf(unrecognisedBanners)) { (base, decorate) =>
      TitleContainment.decorates(tokens(base), tokens(decorate(base))) shouldBe true
      val rows = Seq(resolved(base, 1, 2026, Helios), unresolved(decorate(base), None, Multikino))
      withClue(s"films: ${films(rows).map(_.map(_._1))}\n") {
        sameFilm(rows, rows(0)._1, rows(1)._1) shouldBe true
      }
    }
  }

  // ── Director credits ──────────────────────────────────────────────────────

  private val multiTokenNames: Seq[String] = Seq(
    "Enyedi Ildikó", "Dag Johan Haugerud", "Michel Franco", "Jan Sobierajski", "Yann Gozlan",
    "Bong Joon Ho", "Alejandro González Iñárritu", "Francis Lawrence", "Szabó István", "Pálfi György",
    "Agnieszka Holland", "Neele Leana Vollmar")

  private val genReordered: Gen[(String, String)] = for {
    name <- Gen.oneOf(multiTokenNames)
    seed <- Gen.long
  } yield name -> permute(seed, name.split(" ").toSeq).mkString(" ")

  "a director credit with its name tokens reordered" should "name the same person" in {
    forAll(genReordered) { case (name, reordered) =>
      SamePerson(name, reordered) shouldBe true
      SamePerson(reordered, name) shouldBe true
    }
  }

  it should "leave the verdict on any candidate unchanged" in {
    forAll(genFilmEvidence, genCandidate, genReordered, Gen.oneOf(true, false)) { case (evidence, candidate, (name, reordered), onCrew) =>
      val crewed = if (onCrew) candidate.copy(crew = candidate.crew :+ name) else candidate
      Verdict.of(evidence.withDirectors(Seq(reordered)), crewed) shouldBe Verdict.of(evidence.withDirectors(Seq(name)), crewed)
    }
  }

  // ── Corroborating evidence ────────────────────────────────────────────────

  "evidence that agrees with a candidate" should "never turn an accepted candidate into a rejected one" in {
    forAll(genFilmEvidence, genCandidate, Gen.choose(0, 2)) { (evidence, candidate, which) =>
      whenever(!Verdict.of(evidence, candidate).isReject) {
        val corroborated: FilmEvidence = which match {
          case 0 => candidate.crew.headOption.fold(evidence)(c => evidence.withDirectors(Seq(c)))
          case 1 => candidate.runtime.fold(evidence)(r => evidence.copy(runtimes = (evidence.runtimes :+ r).distinct.sorted))
          case _ => candidate.year.fold(evidence)(y => evidence.copy(years = (evidence.years :+ y).distinct.sorted))
        }
        Verdict.of(corroborated, candidate).isReject shouldBe false
      }
    }
  }

  /** A film's rows as a venue-silent chain lists them (title only), each resolved or
   *  not, keyed at the film's year or yearless, under assorted recognised decorations. */
  private val genSilentFilm: Gen[Seq[Row]] = for {
    base      <- Gen.oneOf(decoratedBases)
    n         <- Gen.choose(1, 4)
    variants  <- Gen.listOfN(n, Gen.oneOf(recognisedDecorations).map(_(base)))
    years     <- Gen.listOfN(n, Gen.option(Gen.const(2026)))
    resolvedN <- Gen.choose(0, n - 1)
  } yield {
    val cinemas = Seq(Multikino, KinoMuza, KinoApollo, Kinoteka)
    (resolved(base, 7, 2026, Helios) +:
      variants.zip(years).zipWithIndex.map { case ((title, year), i) =>
        if (i < resolvedN) resolved(title, 7, 2026, cinemas(i)) else unresolved(title, year, cinemas(i))
      }).distinctBy(_._1)
  }

  "a venue publishing the resolved film's own facts" should "never split a row off the film it is already in" in {
    val runtime = 124
    forAll(genSilentFilm, Gen.long) { (rows, seed) =>
      val before = films(rows).map(_.map(_._1).toSet).toSet
      val target = permute(seed, rows).head
      // The film's TMDB facts, now published by one more venue on one of its rows.
      val facts = SourceData(title = Some(target._1.cleanTitle), releaseYear = Some(2026),
        runtimeMinutes = Some(runtime), director = Seq("Jan Kowalski"))
      val withTmdb = rows.map { case (k, r) =>
        k -> r.copy(data = r.data.map {
          case (Tmdb, sd) => (Tmdb: Source) -> sd.copy(runtimeMinutes = Some(runtime), director = Seq("Jan Kowalski"))
          case other      => other
        })
      }
      val corroborated = withTmdb.map { case (k, r) =>
        if (k == target._1) k -> r.copy(data = r.data + ((KinoPodBaranami: Source) -> facts)) else k -> r
      }
      withClue(s"target=${target._1}\nbefore=$before\n") {
        films(corroborated).map(_.map(_._1).toSet).toSet shouldBe films(withTmdb).map(_.map(_._1).toSet).toSet
      }
    }
  }
}
