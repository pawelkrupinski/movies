package services.movies

import models.Country

/**
 * The rule set a SINGLE-COUNTRY spec in `common` keys under — see the fuller
 * note on the `testkit` copy of this helper.
 *
 * Duplicated deliberately: `testkit` depends on `common`, so `common`'s own test
 * sources cannot see it, and there is no third module both could share. The two
 * definitions are one expression; if that expression ever grows
 * logic, move it into `common/src/main` and have both delegate.
 */
object SingleCountryNormalizer {
  /** A NEW normalizer per call — see the `testkit` copy. */
  def titleNormalizer: TitleNormalizer = TitleNormalizer.forCountry(Country.default)
}
