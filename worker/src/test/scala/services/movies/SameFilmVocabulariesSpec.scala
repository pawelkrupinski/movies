package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{Candidate, Contradiction, Support, Verdict}

/**
 * "Are these two listings the same film?" is answered by three vocabularies, and
 * this spec pins what each says over ONE table of pairs, so the differences are on
 * record rather than rediscovered:
 *
 *   - `Verdict.of` — a CANDIDATE film (TMDB's crew and runtime) against what the
 *     cinemas published: a credited name settles it, then minutes a CATEGORY apart
 *     deny (`RuntimeCorroboration.plausible`, half to double). It never reads a
 *     title, a year or an id.
 *   - `MixedFilmDetector.describeDifferentFilms` — two ROWS' cinema evidence:
 *     disjoint original-title words (or a sequel numeral), corroborated by runtime
 *     (±2 minutes) or, when a side has no minutes, by year
 *     (`YearWindow.PublishedAdjacency`), vetoed by an agreeing whole-name director.
 *     It never reads an id.
 *   - `ScrapeLanding.chooseConcluded` — which of several SAME-TITLED concluded rows a
 *     listing belongs on: the runtime it published against each film's own, then
 *     the film more venues screen, then where this venue already sits, then
 *     `canonicalRank`. It never reads a director or a title.
 *
 * They are not one rule in three coats, and the runtime arms alone say why: 120
 * against 103 minutes is two films to the detector and a plausible candidate to the
 * verdict, and both are right for the question each answers — the detector splits a
 * row, the verdict vetoes a resolution. Folding one onto another would change an
 * answer in this table.
 */
class SameFilmVocabulariesSpec extends AnyFlatSpec with Matchers {

  /** A resolved row: TMDB's own slot plus what ONE cinema published for it. */
  private def row(tmdbId: Int, cinema: Source, title: String, originalTitle: String, year: Int,
                  runtime: Option[Int], director: Seq[String] = Nil, imdbId: Option[String] = None): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), imdbId = imdbId, data = Map[Source, SourceData](
      Tmdb   -> SourceData(title = Some(title), originalTitle = Some(originalTitle), releaseYear = Some(year),
                           runtimeMinutes = runtime, director = director),
      cinema -> SourceData(title = Some(title), originalTitle = Some(originalTitle), releaseYear = Some(year),
                           runtimeMinutes = runtime, director = director)))

  private case class Pair(name: String, a: MovieRecord, b: MovieRecord, different: Boolean, verdict: Verdict)

  private val pairs = Seq(
    Pair("same title, same year, runtimes agree",
      row(1, Multikino, "Twoje imię", "Kimi no na wa", 2016, Some(106)),
      row(1, Helios,    "Twoje imię", "Kimi no na wa", 2016, Some(106)),
      different = false, verdict = Verdict.Accept(Support.Runtime)),
    Pair("same director credited either way round, translated titles, runtimes drift",
      row(1, Multikino,   "Twoje imię", "Kimi no na wa",            2016, Some(110), Seq("Makoto Shinkai")),
      row(1, KinoMuranow, "Twoje imię", "Your Name (re-release)",   2016, Some(83),  Seq("Shinkai Makoto")),
      different = false, verdict = Verdict.Accept(Support.Crew)),
    Pair("same imdbId under two tmdbIds — neither vocabulary reads the id; the settle's imdbId fold does",
      row(1568069, KinoMuza, "Ghost: Rite Here Rite Now", "Ghost: Rite Here Rite Now", 2025, Some(120), imdbId = Some("tt43683692")),
      row(1693400, Helios,   "Ghost: Rite Here Rite Now", "Ghost: Rite Here Rite Now", 2026, Some(120), imdbId = Some("tt43683692")),
      different = false, verdict = Verdict.Accept(Support.Runtime)),
    Pair("decorated title, spelling variant",
      row(1, Multikino, "Terminator 2", "Terminator 2: Judgment Day",               1991, Some(137)),
      row(1, Helios,    "Terminator 2", "Terminator 2: Judgement Day (re-release)", 1991, Some(137)),
      different = false, verdict = Verdict.Accept(Support.Runtime)),
    Pair("remake at a different year, the newer without minutes — the verdict cannot see a year",
      row(1, KinoMuranow, "Joanna d'Arc", "Joan of Arc",    1999, Some(160)),
      row(2, Helios,      "Joanna d'Arc", "Jóhanna af Örk", 2025, None),
      different = true, verdict = Verdict.Insufficient),
    Pair("Cyrillic alias of one film — no word bridges the scripts, the agreeing runtime does",
      row(1, KinoMuranow, "Dzień objawienia", "День істини",      2025, Some(96)),
      row(1, Multikino,   "Dzień objawienia", "Dzień objawienia", 2025, Some(96)),
      different = false, verdict = Verdict.Accept(Support.Runtime)),
    Pair("two films under one Polish title, 17 minutes apart, directors credited",
      row(1, Multikino,   "Obcy", "L'étranger",       2025, Some(120), Seq("François Ozon")),
      row(2, KinoMuranow, "Obcy", "I Was A Stranger", 2025, Some(103), Seq("Brandt Andersen")),
      different = true, verdict = Verdict.Reject(Contradiction.Director)),
    Pair("two films under one Polish title, 17 minutes apart, nobody credited — the runtime arms disagree",
      row(1, Multikino,   "Obcy", "L'étranger",       2025, Some(120)),
      row(2, KinoMuranow, "Obcy", "I Was A Stranger", 2025, Some(103)),
      different = true, verdict = Verdict.Accept(Support.Runtime)))

  "describeDifferentFilms and Verdict.of" should "each keep their own answer over the table" in {
    pairs.foreach { p =>
      withClue(s"[${p.name}] describeDifferentFilms: ") {
        MixedFilmDetector.describeDifferentFilms(p.a, p.b, titleNormalizer) shouldBe p.different
      }
      withClue(s"[${p.name}] Verdict.of(a's cinemas, b's film): ") {
        Verdict.of(p.a.evidence, Candidate.fromSlot(p.b.tmdbId.get, p.b.data(Tmdb))) shouldBe p.verdict
      }
    }
  }

  // ── chooseConcluded ─────────────────────────────────────────────────────────

  private val title = "Tylko jedna noc"

  /** Antonioni's 1961 "La notte" (121 min) and the 2026 romcom (102 min), both
   *  concluded under one Polish title, then a YEARLESS Helios listing: the row it
   *  lands on. `extraNewVenues` puts more venues on the 2026 row first. */
  private def landing(listingRuntime: Option[Int], extraNewVenues: Seq[Cinema] = Nil): Option[Int] = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    cache.put(cache.keyOf(title, Some(1961)), row(1, KinoMuranow, title, "La notte", 1961, Some(121)))
    cache.put(cache.keyOf(title, Some(2026)), MovieRecord(tmdbId = Some(2), data =
      (Multikino +: extraNewVenues).map(c => (c: Source) -> SourceData(title = Some(title), releaseYear = Some(2026))).toMap +
        (Tmdb -> SourceData(title = Some(title), originalTitle = Some("One Night Only"), releaseYear = Some(2026), runtimeMinutes = Some(102)))))

    cache.recordCinemaScrape(Helios, Seq(CinemaMovie(Movie(title = title, runtimeMinutes = listingRuntime), Helios,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil)))

    Seq(1961, 2026).find(y => cache.get(cache.keyOf(title, Some(y))).exists(_.cinemaData.contains(Helios)))
  }

  "chooseConcluded" should "route a yearless listing by the minutes it published" in {
    landing(Some(105)) shouldBe Some(2026)
    landing(Some(118)) shouldBe Some(1961)
  }

  it should "route a listing with no minutes to the film more venues screen" in {
    landing(None, extraNewVenues = Seq(CinemaCityArkadia)) shouldBe Some(2026)
  }

  // The tie: no minutes, one venue each, this venue on neither. Nothing the listing
  // or the corpus says separates the films, and the pick is `canonicalRank`'s — the
  // lower year. Pinned as what it IS, not as what it should be.
  it should "fall back to canonicalRank when nothing separates the films" in {
    landing(None) shouldBe Some(1961)
  }
}
