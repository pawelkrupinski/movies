package tools

import services.movies.TitleNormalizer
import services.scrapes.ArchivedScrape

import scala.util.Random

/**
 * A small, representative slice of a country's corpus — the input to the fast
 * convergence leg.
 *
 * The full legs are the authority and take 12 to 73 minutes to become one. That is
 * far too slow to be the first thing that notices a broken enrichment chain: a
 * fallback that turned a 404 into an outage cost all three countries their Metacritic
 * and Rotten Tomatoes ladders, and the evidence only arrived after the whole matrix
 * had run. A hundred films reach the same code over the same shapes in a couple of
 * minutes.
 *
 * Sampled by FILM, not by venue. Taking the first N venues would bias the slice
 * towards whichever chains sort early and would drop the multi-venue films that make
 * folding interesting; taking N listings at random would mostly take one cinema's
 * copy of a film and lose the cross-venue merge entirely. Picking films and then
 * keeping every listing of them preserves both — a sampled film arrives from all the
 * cinemas that report it, spelt however each of them spells it.
 */
object CorpusSample {

  /** How many films the fast leg replays. Big enough to carry a real mix — chains and
   *  independents, resolved and unresolvable, decorated and bare — and small enough
   *  that the leg is a couple of minutes rather than an hour. */
  val DefaultSize = 100

  /** The sanitized key every listing of a film shares — the same identity
   *  `MovieCache` folds on, so "Diuna", "DIUNA" and "Diuna (dubbing)" are one pick
   *  rather than three. */
  private def keyOf(film: models.CinemaMovie, normalizer: TitleNormalizer): String =
    normalizer.sanitize(film.movie.title)

  /** The CINEMA SLOT KEY of every listing of the sampled films, across every venue.
   *
   *  This is the join to production. A sampled film cannot be matched to a prod row by
   *  `_id`: that is the sanitize of the FOLDED display title, while the corpus carries
   *  what each cinema actually wrote. Matching the two collapsed 100 sampled films to
   *  58 prod rows — and biased the survivors towards the ones that folded cleanly,
   *  which are disproportionately the well-known resolvable ones, so the baseline came
   *  out reading 79% resolved against the replay's 70%.
   *
   *  The slot KEY is what both sides derive from the same (cinema, title) pair, through
   *  the same `CinemaShowing.keyFor` — so there is one rule, not two that have to agree.
   *  The slot's stored TITLE is not that: prod strips a listing's decoration before
   *  storing it, so a corpus "The Room [dubbing]" never equalled the "The Room" prod had
   *  put on the slot, and every decorated listing silently fell out of the baseline —
   *  precisely the shapes this sample is drawn to over-represent. */
  def slotKeysOf(rows: Seq[ArchivedScrape], keys: Set[String], normalizer: TitleNormalizer): Set[String] =
    rows.flatMap(row => row.films
        .filter(film => keys.contains(keyOf(film, normalizer)))
        .map(film => models.CinemaShowing.keyFor(row.cinema, film.movie.title, normalizer).displayName))
      .toSet

  /** Every distinct film key in the corpus, sorted — a stable universe to sample
   *  from, so the only randomness is the draw itself. */
  def filmKeys(rows: Seq[ArchivedScrape], normalizer: TitleNormalizer): Seq[String] =
    rows.flatMap(_.films.map(keyOf(_, normalizer))).filter(_.nonEmpty).distinct.sorted

  /**
   * Draw `size` film keys using `random`.
   *
   * The draw happens once, at RECORDING time, and the result is pinned by the file it
   * writes — so the leg that replays it is perfectly reproducible while the slice
   * still rotates as the corpus is re-recorded. A fixed seed would freeze the same
   * hundred films for ever and stop covering anything new; drawing per-run and
   * committing the outcome gets both properties.
   */
  def pick(rows: Seq[ArchivedScrape], size: Int, random: Random, normalizer: TitleNormalizer): Set[String] = {
    val universe = filmKeys(rows, normalizer)
    if (universe.sizeIs <= size) universe.toSet else random.shuffle(universe).take(size).toSet
  }

  /**
   * Keep only the listings for `keys`, and only the venues left holding any.
   *
   * A venue whose every film was dropped is removed rather than kept empty: the
   * replay's no-loss assertion compares the cinemas the archive holds against the
   * cinemas the read model emits, and an empty row would claim a cinema that has
   * nothing to emit. Everything else about a row — its city, its scrape instant, its
   * `listingComplete` flag — is carried through untouched, because the sample has to
   * be a corpus in exactly the same sense the whole one is.
   */
  def trim(rows: Seq[ArchivedScrape], keys: Set[String], normalizer: TitleNormalizer): Seq[ArchivedScrape] =
    rows.flatMap { row =>
      // `films` reads through `lastSuccess`, so the filter has to land there — that is
      // also what keeps the scrape's own `at` and `listingComplete` attached to the
      // listing they describe.
      row.lastSuccess.map(success => row.copy(
        lastSuccess = Some(success.copy(films = success.films.filter(film => keys.contains(keyOf(film, normalizer)))))))
    }.filter(_.films.nonEmpty)
}
