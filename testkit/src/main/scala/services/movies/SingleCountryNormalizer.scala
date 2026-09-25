package services.movies

import models.Country

/**
 * The rule set a SINGLE-COUNTRY spec keys under. The overwhelming majority of
 * specs are not about which country's rules fold a title — they just need keys
 * to behave the way they always did — so they call this.
 *
 * A spec that IS about country scoping must NOT use it: build the instances
 * explicitly and hold two at once, the way `TitleNormalizerInstanceSpec` does.
 *
 * `common`'s own tests cannot see `testkit` (testkit depends on common), so an
 * identical helper lives in `common/src/test`; there is no module both can share.
 */
object SingleCountryNormalizer {
  /** A NEW Poland normalizer per call — never one every spec in the JVM shares,
   *  since each instance fills its own memo caches. Building one costs ~1.3ms, so
   *  a spec that keys many titles holds the one it got. */
  def titleNormalizer: TitleNormalizer = TitleNormalizer.forCountry(Country.default)
}
