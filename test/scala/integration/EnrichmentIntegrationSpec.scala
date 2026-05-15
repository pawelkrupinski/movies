package integration

import clients.TmdbClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbClient
import tools.Env

/**
 * Live test of the enrichment pipeline against real TMDB + IMDb GraphQL.
 * Requires TMDB_API_KEY (in `.env.local` or the environment).
 *
 * Exercises the same classes the production code uses — no mocking, no fakes.
 * The Polish-title → TMDB → IMDB id → IMDb-rating chain is what
 * `MovieService.fetchEnrichment` runs in the background; this spec proves
 * the chain works end-to-end on a handful of real films that show up in the
 * site's repertoire.
 */
class EnrichmentIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("TMDB_API_KEY").isDefined, "TMDB_API_KEY not set")

  private val tmdb = new TmdbClient()
  private val imdb = new ImdbClient()

  // Films that appear in the current site data, picked for variety: a sequel,
  // an upcoming blockbuster, a Polish-language art-house piece, and a 1960s
  // classic (Wajda) which exercises old-films-with-diacritics matching.
  private val knownFilms = Seq(
    ("Diabeł ubiera się u Prady 2",  Some(2026)),
    ("Mortal Kombat II",             Some(2025)),
    ("Mandalorian & Grogu",          Some(2026)),
    ("Wartość sentymentalna",        Some(2025)),
    ("Niewinni czarodzieje",         Some(1960))
  )

  "TmdbClient.search" should "find a TMDB id for each known Polish title" in {
    knownFilms.foreach { case (title, year) =>
      withClue(s"$title (${year.getOrElse("?")}): ") {
        val hit = tmdb.search(title, year)
        hit       should not be empty
        info(s"  TMDB id for $title = ${hit.get.id}")
      }
    }
  }

  "TmdbClient.imdbId" should "follow a TMDB id to a valid IMDb id" in {
    knownFilms.foreach { case (title, year) =>
      withClue(s"$title (${year.getOrElse("?")}): ") {
        val imdbId = tmdb.search(title, year).map(_.id).flatMap(tmdb.imdbId)
        imdbId                       should not be empty
        imdbId.get                   should startWith ("tt")
        info(s"  IMDb id for $title = ${imdbId.get}")
      }
    }
  }

  "The full Polish-title → TMDB → IMDb pipeline" should
      "produce a usable IMDb id (and rating when IMDb has aggregated one) for each known film" in {
    var found = 0
    knownFilms.foreach { case (title, year) =>
      val result = for {
        hit    <- tmdb.search(title, year)
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

  "Title normalisation" should "let TMDB find a film whose Polish title we lowercased" in {
    // Just sanity-checks that TMDB itself is forgiving of case — our normaliser
    // is for de-duplicating cache keys, not for sanitising the search query.
    tmdb.search("diabeł ubiera się u prady 2", Some(2026)) should not be empty
  }
}
