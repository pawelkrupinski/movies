package scripts

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/** The scripts that open their store from the configuration (EnrichmentBackfill, DuplicateAudit)
 *  key its rows by the CONFIGURED country's rules: a German corpus's `_id`s were sanitized by
 *  Germany's, and Poland's " & " -> " i " unification would split or collide them. */
class AmbientMovieRepositorySpec extends AnyFlatSpec with Matchers {

  private val title = "Wallace & Gromit"

  "AmbientMovieRepository.open" should "key the store by the configured country's title rules, not Poland's" in {
    val german = AmbientMovieRepository.open(new _root_.settings.ProcessConfiguration(_root_.tools.Env.of("KINOWO_COUNTRY" -> "de")), _root_.tools.SpecClock.Pinned)
    try {
      // The positive control: the two countries' rules do key this title apart.
      TitleNormalizer.forCountry(Country.Germany).sanitize(title) should not be TitleNormalizer.forCountry(Country.Poland).sanitize(title)
      german.normalizer.sanitize(title) shouldBe TitleNormalizer.forCountry(Country.Germany).sanitize(title)
    } finally german.close()
  }
}
