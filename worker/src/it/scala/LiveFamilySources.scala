package integration

import services.identity._
import services.identity.agreement.VoterFamily
import tools.HttpFetch

/** The film-database families the agreement stage asks when a capture, a fill or the resolver-only dump answers its
 *  questions — over `fetch` (the signal-combination experiment's answer cache, else live), `families` only. Filmweb's
 *  programmes are named by today's date in Poland, which only the real clock names (the answer stores keep their pinned
 *  one). */
object LiveFamilySources {
  def of(fetch: HttpFetch, country: models.Country, families: Seq[VoterFamily]): Map[VoterFamily, FamilySource] =
    Seq(new ImdbFamily(new services.enrichment.ImdbClient(fetch)),
      new WikiFamily(new services.enrichment.WikidataClient(fetch), country.language.getLanguage),
      new FilmwebFamily(new services.enrichment.FilmwebClient(fetch),
        services.cinemas.pl.FilmwebProgrammes.resolving(fetch, () => new models.VenueClock(java.time.Clock.systemUTC()).todayInPoland).of),
      new RottenTomatoesFamily(new services.enrichment.RottenTomatoesClient(fetch)),
      new MetacriticFamily(new services.enrichment.MetacriticClient(fetch))).filter(source => families.contains(source.family)).map(s => s.family -> s).toMap
}
