package services.movies

import models.Country

/**
 * The rule set a SINGLE-COUNTRY spec keys under.
 *
 * `CacheKey` takes its normalizer as a context parameter now, because a title's
 * identity depends on whose rules folded it. The overwhelming majority of specs
 * are not about that distinction — they just need keys to behave the way they
 * always did — so they import this given and read unchanged.
 *
 * A spec that IS about country scoping must NOT import it: build the instances
 * explicitly and hold two at once, the way `TitleNormalizerInstanceSpec` does.
 * An ambient given would quietly make such a spec assert against one country
 * while claiming to compare two.
 *
 * `common`'s own tests cannot see `testkit` (testkit depends on common), so an
 * identical helper lives in `common/src/test`; there is no module both can share.
 */
object SingleCountryNormalizer {
  given TitleNormalizer = TitleNormalizer.forCountry(Country.default)
}
