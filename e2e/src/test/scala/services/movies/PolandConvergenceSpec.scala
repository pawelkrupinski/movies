package services.movies

import models.Country

/** Poland's pipeline reaches a fixpoint on its own catalogue and title rules.
 *  Complements `ReScrapeIdempotencySpec`, which asks the same question of the
 *  recorded HTTP corpus — this one asks it of the archive replay path, so the two
 *  fail for different reasons. */
@CorpusReplay @CountryScoped
class PolandConvergenceSpec extends CountryConvergenceBehaviour(Country.Poland, corpusKey = "pl")
