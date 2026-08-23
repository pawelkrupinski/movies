package integration

import clients.TmdbClient
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbClient
import tools.{Env, RealHttpFetch}

/**
 * Live test of the enrichment pipeline against real TMDB + IMDb GraphQL.
 * Requires TMDB_API_KEY (in `.env.local` or the environment).
 *
 * Exercises the same classes the production code uses — no mocking, no fakes.
 * The Polish-title → TMDB → IMDB id → IMDb-rating chain is what
 * `MovieService.fetchEnrichment` runs in the background; this spec proves
 * the chain works end-to-end on a handful of real films that show up in the
 * site's repertoire.
 *
 * Shared TMDB/IMDb clients live on the companion so `ParallelTestExecution`'s
 * one-instance-per-test doesn't multiply the HTTP-client setup.
 */
object EnrichmentIntegrationSpec {
  private val tmdb = new TmdbClient(new RealHttpFetch)
  private val imdb = new ImdbClient(new RealHttpFetch)
}

class EnrichmentIntegrationSpec extends AnyFlatSpec with Matchers with ParallelTestExecution {

  assume(Env.get("TMDB_API_KEY").isDefined, "TMDB_API_KEY not set")

  import EnrichmentIntegrationSpec.{tmdb, imdb}

  // TMDB and IMDb both answer `None` when they throttle us, exactly as they do when a
  // film genuinely is not there — so a rate limiter looked identical to a broken lookup
  // and took seven cases down together on 2026-07-28. `orCancel` retries the burst, then
  // asks the upstream whether it is answering at all before calling it a regression.
  private def probe(url: String): () => Unit = () => { new RealHttpFetch().get(url); () }
  private def viaTmdb[T](body: => T): T =
    LiveUpstream.orCancel("TMDB", probe(LiveUpstream.Probes.tmdb(Env.get("TMDB_API_KEY").get)))(body)
  private def viaImdb[T](body: => T): T =
    LiveUpstream.orCancel("IMDb", probe(LiveUpstream.Probes.Imdb))(body)

  /** Guarded by BOTH, because the pipeline case needs both.
   *
   *  It reaches IMDb only THROUGH TMDB — search the title, follow the id — so
   *  guarding it on IMDb alone left it failing whenever TMDB was the upstream that
   *  was down: the `viaTmdb` cases beside it cancelled correctly while this one ran
   *  on an unreachable TMDB, resolved nothing, and failed its `found >= 1`. Observed
   *  2026-08-02 with six TMDB cases cancelled and this one red in the same run —
   *  which reads as a pipeline regression and is an upstream outage. */
  private def viaTmdbAndImdb[T](body: => T): T = viaTmdb(viaImdb(body))

  // Films that appear in the current site data, picked for variety: a sequel,
  // an upcoming blockbuster, a Polish-language art-house piece, and a 1960s
  // classic (Wajda) which exercises old-films-with-diacritics matching.
  private val knownFilms = Seq(
    ("Diabeł ubiera się u Prady 2",  Some(2026)),
    // 2026, not 2025 — TMDB stores the theatrical date (931285, 2026-05-06) and a
    // year-scoped search for 2025 returns nothing at all. The wrong year survived here
    // only because `TmdbClient.search`'s year-less retry rescued it, and production has
    // never resolved through that method; the live path refuses a wrong year outright.
    ("Mortal Kombat II",             Some(2026)),
    // The spelling Polish cinemas actually list (and the archive carries), not the
    // English marketing title: TMDB indexes it as "Gwiezdne wojny: Mandalorian i Grogu",
    // and a search for "Mandalorian & Grogu" returns the film PLUS a Disney+ featurette,
    // which the singleton rule refuses — correctly, on a title no exhibitor emits.
    ("Gwiezdne wojny: Mandalorian i Grogu", Some(2026)),
    ("Wartość sentymentalna",        Some(2025)),
    ("Niewinni czarodzieje",         Some(1960))
  )

  /** What production resolves a title through: the strict singleton rule first, then a
   *  year-scoped search whose top hit matches the title verbatim. `TmdbClient.search`
   *  used to stand in for this here, but no production path has ever called it — and a
   *  LIVE spec asserting that real Polish titles resolve is worth nothing if it resolves
   *  them by a route the worker cannot take. Mirrors `MovieService.resolveTmdbId`. */
  private def resolve(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] =
    tmdb.searchUnique(title, year).orElse(tmdb.searchYearExactTop(title, year))

  "TMDB resolution" should "find a TMDB id for each known Polish title" in viaTmdb {
    knownFilms.foreach { case (title, year) =>
      withClue(s"$title (${year.getOrElse("?")}): ") {
        val hit = resolve(title, year)
        hit       should not be empty
        info(s"  TMDB id for $title = ${hit.get.id}")
      }
    }
  }

  "TmdbClient.imdbId" should "follow a TMDB id to a valid IMDb id" in viaTmdb {
    knownFilms.foreach { case (title, year) =>
      withClue(s"$title (${year.getOrElse("?")}): ") {
        val imdbId = resolve(title, year).map(_.id).flatMap(tmdb.imdbId)
        imdbId                       should not be empty
        imdbId.get                   should startWith ("tt")
        info(s"  IMDb id for $title = ${imdbId.get}")
      }
    }
  }

  "The full Polish-title → TMDB → IMDb pipeline" should
      "produce a usable IMDb id (and rating when IMDb has aggregated one) for each known film" in viaTmdbAndImdb {
    var found = 0
    knownFilms.foreach { case (title, year) =>
      val result = for {
        hit    <- resolve(title, year)
        imdbId <- tmdb.imdbId(hit.id)
      } yield (hit, imdbId, imdb.lookup(imdbId))

      result match {
        case Some((hit, imdbId, imdbRating)) =>
          found += 1
          info(s"  $title → IMDb=${imdbRating.map(r => f"$r%.1f").getOrElse("—")}, " +
               s"orig='${hit.originalTitle.getOrElse("?")}', $imdbId")
        case None =>
          info(s"  $title → TMDB couldn't resolve it")
      }
    }
    found should be >= 1
  }

  "Title normalisation" should "let TMDB find a film whose Polish title we lowercased" in viaTmdb {
    // Just sanity-checks that TMDB itself is forgiving of case — our normaliser
    // is for de-duplicating cache keys, not for sanitising the search query.
    resolve("diabeł ubiera się u prady 2", Some(2026)) should not be empty
  }
}
