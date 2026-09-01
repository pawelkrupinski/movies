package services.movies

import models.Country

/** Spain's pipeline reaches a fixpoint. Like Germany's, the only replay Spain
 *  has: its 595 SensaCine venues appear in no other recorded HTTP corpus. */
@CorpusReplay @CountryScoped
class SpainConvergenceSpec extends CountryConvergenceBehaviour(Country.Spain, corpusKey = "es")
