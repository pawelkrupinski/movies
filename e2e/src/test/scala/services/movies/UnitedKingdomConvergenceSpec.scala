package services.movies

import models.Country

/** The UK's pipeline reaches a fixpoint. Its chain venues (Cineworld, Odeon, Vue,
 *  Flicks) share films across hundreds of sites, which is the heaviest merge
 *  pressure any country puts on the fold. */
@CorpusReplay @CountryScoped
class UnitedKingdomConvergenceSpec extends CountryConvergenceBehaviour(Country.UnitedKingdom)
