package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SingleCountryNormalizerSpec extends AnyFlatSpec with Matchers {

  "The single-country spec normalizer" should "be a new instance per use, not one every spec in the JVM shares" in {
    // Each instance fills its own memo caches; a shared one let every spec fill the same.
    SingleCountryNormalizer.titleNormalizer should not be theSameInstanceAs (SingleCountryNormalizer.titleNormalizer)
  }
}
