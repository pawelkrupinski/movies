package services.movies

import models.Country

/** The FAST leg: the same convergence claims over the ~100-film sample recorded
 *  beside Spain's corpus.
 *
 *  Runs before the full leg in the matrix so a regression that costs every country
 *  its rating ladders is caught in a couple of minutes rather than after the
 *  longest leg in the suite. Same assertions, smaller corpus — see
 *  `CountryConvergenceBehaviour`'s `corpusKey`. */
@CorpusReplay @CountryScoped
class SpainSampleConvergenceSpec extends CountryConvergenceBehaviour(Country.Spain, corpusKey = "es-sample")
