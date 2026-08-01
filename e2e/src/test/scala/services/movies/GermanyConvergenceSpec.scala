package services.movies

import models.Country

/** Germany's pipeline reaches a fixpoint. The only replay Germany has: its 1,533
 *  Filmstarts venues appear in no recorded HTTP corpus. */
@CorpusReplay @CountryScoped
class GermanyConvergenceSpec extends CountryConvergenceBehaviour(Country.Germany, corpusKey = "de")
