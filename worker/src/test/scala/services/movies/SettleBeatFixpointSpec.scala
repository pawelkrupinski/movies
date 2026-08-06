package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.{InMemoryStagingFolder, InMemoryStagingRepository}

/**
 * Loop A — the settle-beat oscillation — reproduced at the layer that actually turns:
 * scrape → settle → fold → hydrate, driven with a CONSTANT set of cinema listings.
 *
 * Production PL merges rows on every `SettleReaper` tick (:21 and :51, ~83
 * `merges_total{reason="canonicalize"}` a day) and re-requests Filmweb/Metacritic/RT for
 * each looping film every cycle. The input does not change between those ticks — the same
 * cinemas list the same films — so a healthy pipeline must reach a FIXPOINT: one more
 * cycle over unchanged listings changes nothing.
 *
 * The state seeded below is the prod fold state (the same one `UnknownBannerReDivertSpec`
 * documents): ONE `movies` row keyed by the bare display title, carrying both the bare
 * venues' slots and the one venue that publishes a decorated spelling.
 *
 * This path CONVERGES, and pinning that is the point. The film re-keys onto the decorated
 * spelling on the gap tick, the returning venues re-divert, and the fold merges them back —
 * settled again two cycles later. So the oscillation prod actually runs is NOT reachable
 * from here, and a green run of this spec is not evidence that loop A is fixed: the
 * disagreement that sustains it needs the STORAGE SPLIT, where the fold reads `movies` raw
 * and a migrated film reports no cinemas at all. That is
 * `integration.FoldSpellingAgreesWithSettleSpec`, and it is the regression test for the fix.
 * Believing this layer covered it is what sent an earlier attempt after the wrong pool.
 *
 * It asserts the fixpoint rather than a guessed path through the loop, so when it fails it
 * prints the per-cycle key sets rather than only the mismatch.
 */
class SettleBeatFixpointSpec extends AnyFlatSpec with Matchers {

  private val bare      = "Arek. Mama. Panorama"
  private val decorated = "Przedpremiera: Arek. Mama. Panorama | Wakacje z dokumentem"

  // Guard: the two really are distinct keys, so the drift below is real rather than an
  // artefact of a rule that already collapses them.
  require(titleNormalizer.sanitize(bare) != titleNormalizer.sanitize(decorated),
    "the decorated form is already collapsed by a rule — pick another to keep this honest")

  private val Year = Some(2026)

  /** Twelve venues publish the film plainly; one dresses it up. */
  private val bareVenues: Seq[Cinema] = Seq(
    KinoApollo, KinoBulgarska, CharlieMonroe, Helios, CinemaCityKinepolis, KinoMuza,
    Multikino, KinoPalacowe, CinemaCityPoznanPlaza, Rialto, CinemaCityWroclavia, CinemaCityKorona)
  private val decoratedVenue: Cinema = MultikinoPasazGrunwaldzki

  private def listing(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = Year), cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil)

  /** The prod fold state: one row, keyed bare, holding every venue's slot. */
  private def foldedRow: MovieRecord =
    MovieRecord(data =
      (bareVenues.map(c => (c: Source) -> SourceData(
        title = Some(bare), rawTitle = Some(bare), releaseYear = Year)) :+
        ((decoratedVenue: Source) -> SourceData(
          title = Some(decorated), rawTitle = Some(decorated), releaseYear = Year))).toMap)

  /** Something else on the bare venues' boards, so the gap cycle below is a real listing
   *  (an empty one is treated as a scraper failure and bails before the prune). */
  private val otherFilm = "Wielka Podróż Kapitana Nemo"

  "the pipeline" should "settle again after a film briefly drops off the bare venues' boards" in {
    val repository = new InMemoryMovieRepository
    val staging    = new InMemoryStagingRepository
    val cache      = new CaffeineMovieCache(repository, staging = Some(staging), normalizer = titleNormalizer)
    val folder     = new InMemoryStagingFolder(staging, repository, titleNormalizer)

    cache.put(CacheKey(bare, Year, titleNormalizer), foldedRow)

    /** One worker cycle: every cinema scrapes, the settle canonicalizes, the folder drains
     *  staging, and the change stream catches the cache up on the folder's out-of-band
     *  `movies` writes (`rehydrate`). `bareTitle` is what the twelve plain venues list this
     *  tick — the gap cycle swaps it for another film. */
    def cycle(bareTitle: String): (Set[CacheKey], Set[String]) = {
      bareVenues.foreach(c => cache.recordCinemaScrape(c, Seq(listing(c, bareTitle))))
      cache.recordCinemaScrape(decoratedVenue, Seq(listing(decoratedVenue, decorated)))
      cache.canonicalizeBySanitize()
      staging.findAll().map(_.title).distinct.foreach(t => folder.foldGroup(t))
      cache.rehydrate()
      (cache.entries.map(_._1).toSet, staging.findAll().map(_.title).toSet)
    }

    // Two settled ticks, ONE tick where the film is off the bare venues' boards, then the
    // listings return to constant and stay that way — so everything after cycle 3 is the
    // pipeline reacting to itself, not to changing input.
    val observed = (cycle(bare) +: cycle(bare) +: cycle(otherFilm) +: (4 to 9).map(_ => cycle(bare))).toIndexedSeq
    val keySets  = observed.map(_._1)
    val stagings = observed.map(_._2)

    val trace = observed.zipWithIndex.map { case ((keys, staged), i) =>
      f"  cycle ${i + 1}: movies=${keys.map(k => s"'${k.cleanTitle}'").toSeq.sorted.mkString(", ")}" +
        (if (staged.isEmpty) "  staging=—" else s"  staging=${staged.toSeq.sorted.mkString(", ")}")
    }.mkString("\n")

    withClue(
      "the listings stopped changing after cycle 3, so the corpus must stop moving too — a " +
      "key set that keeps flipping IS the 30-minute settle beat, and every flip re-keys the " +
      s"row and re-requests its ratings:\n$trace\n") {
      keySets.drop(5).distinct should have size 1
      stagings.drop(5).distinct should have size 1
    }
  }
}
