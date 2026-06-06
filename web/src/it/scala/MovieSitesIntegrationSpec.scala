package integration

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{MetacriticClient, RottenTomatoesClient}
import tools.RealHttpFetch

/**
 * Live tests for the URL validators. Catches regressions where Metacritic or
 * Rotten Tomatoes change their slug conventions / start blocking our UA /
 * route the canonical path through Cloudflare instead of returning 404.
 *
 * No keys / no auth — both sites answer plain GETs. Tests are independent
 * (each builds its own client + probe) so `ParallelTestExecution` runs the
 * 12 network probes concurrently.
 *
 * Every assertion runs through `RetryWithBackoff()` so a transient 5xx on
 * MC/RT (they 502 / 503 / Cloudflare-challenge occasionally) doesn't fail
 * the build — `canonicalUrl` swallows any non-2xx as "slug doesn't exist",
 * so a one-off 502 would otherwise collapse a real `Some(...)` to `None`.
 */
class MovieSitesIntegrationSpec extends AnyFlatSpec with Matchers with ParallelTestExecution {

  "MetacriticClient.canonicalUrl" should "resolve The Dark Knight to its canonical page" in {
    val c = new MetacriticClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("The Dark Knight") shouldBe
        Some("https://www.metacritic.com/movie/the-dark-knight")
    }
  }

  it should "return None for a clearly bogus title" in {
    val c = new MetacriticClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("totally not a real movie xyz12345") shouldBe None
    }
  }

  it should "return None via urlFor when canonical 404s (search URLs never persisted)" in {
    val c = new MetacriticClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.urlFor("totally not a real movie xyz12345") shouldBe None
    }
  }

  // Regression: MC keeps `!` in slugs — stripping it produced 404 for
  // Yu-Gi-Oh!, Airplane!, Moulin Rouge!, etc.
  it should "preserve '!' when building the slug (Yu-Gi-Oh!)" in {
    val c = new MetacriticClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("Yu-Gi-Oh! The Dark Side of Dimensions") shouldBe
        Some("https://www.metacritic.com/movie/yu-gi-oh!-the-dark-side-of-dimensions")
    }
  }

  "RottenTomatoesClient.canonicalUrl" should "resolve The Dark Knight" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("The Dark Knight") shouldBe
        Some("https://www.rottentomatoes.com/m/the_dark_knight")
    }
  }

  it should "return None for a clearly bogus title" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("totally not a real movie xyz12345") shouldBe None
    }
  }

  it should "return None via urlFor when canonical 404s (search URLs never persisted)" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.urlFor("totally not a real movie xyz12345") shouldBe None
    }
  }

  // Regression: RT drops the leading "the_" — /m/the_sting 404s but /m/sting
  // is the real page. The candidate-slug logic should follow the redirect.
  it should "resolve 'The Sting' to /m/sting (article-stripped) on RT" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("The Sting") shouldBe Some("https://www.rottentomatoes.com/m/sting")
    }
  }

  it should "resolve 'The Phantom of Liberty' to /m/phantom_of_liberty on RT" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.canonicalUrl("The Phantom of Liberty") shouldBe
        Some("https://www.rottentomatoes.com/m/phantom_of_liberty")
    }
  }

  // ── Live search-page fallback + score scrape ───────────────────────────────
  //
  // These exercise the new RT scraper paths against the real site (regression
  // catch for layout changes — see MetacriticClient's parallel coverage).

  "RottenTomatoesClient.urlFor" should "fall through to the search-page scrape and pick the year-correct cut" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      // "Top Gun" has multiple cuts. Year=1986 should land on /m/top_gun even
      // if the canonical slug probe doesn't immediately disambiguate.
      c.urlFor("Top Gun", None, Some(1986)) shouldBe
        Some("https://www.rottentomatoes.com/m/top_gun")
    }
  }

  "RottenTomatoesClient.scoreFor" should "scrape the Tomatometer for a well-known film" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      // Catastrophe-detection only: assert we got *some* numeric value in a
      // sensible band, not a specific number that RT could legitimately update.
      c.scoreFor("https://www.rottentomatoes.com/m/the_dark_knight")
        .exists(s => s >= 80 && s <= 100) shouldBe true
    }
  }

  it should "return None for a bogus /m/ slug" in {
    val c = new RottenTomatoesClient(new RealHttpFetch)
    RetryWithBackoff() {
      c.scoreFor("https://www.rottentomatoes.com/m/totally_not_a_real_movie_xyz12345") shouldBe None
    }
  }
}
