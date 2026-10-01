package services.movies

import clients.TmdbClient
import models.{CinemaShowing, Country, MovieRecord, OdeonCinemaBridgend, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.ResolutionCache
import tools.RecordedResponses

/**
 * A rerelease listed at its SCREENING year must not resolve to the director's film OF that year.
 *
 * UK prod, 2026-09-08 and 09-17: Odeon's "The Hunger Games: Mockingjay - Part 1 (2026)" (and
 * Part 2, Catching Fire) resolved by director-walk — "Francis Lawrence (person 10943) year=2026 →
 * tmdbId=1300968 'The Hunger Games: Sunrise on the Reaping'" — the one 2026 credit in his
 * filmography, corroborated by the franchise words alone. That wrong id is what the stored
 * `hungergamesballadofsongbirdssnakes|2026` row still carries, and what the staging fold then
 * collided on (`StagingFoldSpec`, "ONE document per tmdbId"). Two guards now refuse the
 * year-pinned credit, each on its own: `SequelMarker.differentInstalments` names the two curated
 * siblings apart (580401009), and `titleNamesAnotherCredit` sees the title is another of his
 * credits (3ddc1a100). Disabling both reproduces the prod log line against this replay.
 *
 * Replayed from the UK hard cluster's RECORDED TMDB answers
 * (`test/resources/fixtures/corpus/hard-clusters-responses-uk.json.gz`), the very search,
 * person and filmography responses that pipeline sees; the venue slot is Odeon Bridgend's as
 * stored in prod `movie_slots`.
 */
class RereleaseYearResolveSpec extends AnyFlatSpec with Matchers {

  private val uk = TitleNormalizer.forCountry(Country.UnitedKingdom)

  "a rerelease listed at its screening year" should "not resolve to the director's film of that year" in {
    val recorded = RecordedResponses.replaying(RecordedResponses.pathFor(Country.UnitedKingdom.code))
    val tmdb     = new TmdbClient(recorded, apiKey = Some(settings.TmdbApiKey("replay")), language = Country.UnitedKingdom.language)
    val search   = new TmdbCandidateSearch(tmdb, uk, ResolutionCache.passthrough, letterboxdIdResolver = None, wikidata = None)
    val title    = "The Hunger Games: Mockingjay - Part 1 (2026)"
    val odeon = SourceData(title = Some(title), rawTitle = Some(title),
      synopsis = Some("Katniss Everdeen (Jennifer Lawrence) is rescued by the rebels and brought to District 13 after " +
        "she shatters the Hunger Games forever. Each Hunger Games re-release will also include a different exclusive " +
        "theatrical sneak peek at The Hunger Games: Sunrise on the Reaping, releasing 20/11/2026."),
      cast = Seq("Jennifer Lawrence", "Donald Sutherland", "Liam Hemsworth", "Josh Hutcherson"),
      director = Seq("Francis Lawrence"), runtimeMinutes = Some(123))
    val row = MovieRecord(data = Map[Source, SourceData](CinemaShowing(OdeonCinemaBridgend, "thehungergamesmockingjaypart1") -> odeon))

    val resolved = search.resolve(title, Some(2026), row, director = Some("Francis Lawrence")).map(_._1)

    resolved should not contain 1300968
    // A lone rerelease row is left unresolved rather than guessed; the fold reclaims it onto the
    // 2014 film (`expected-hard-clusters-uk.txt`: Odeon Birmingham on 131631).
    resolved shouldBe None
    withClue(s"requests the recording does not hold: ${recorded.missedKeys}\n")(recorded.misses shouldBe 0)
  }
}
