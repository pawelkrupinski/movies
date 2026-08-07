package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.SourceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Loop A — the 30-minute settle beat — at the seam that causes it.
 *
 * A film's identity spelling is a plurality vote over the cinema titles on the row
 * (`FilmCanonicalizer.canonical` → `MovieRecord.displayTitle` → `chooseDisplay`). TWO
 * components run that vote, and under the storage split they did not see the same pool:
 *
 *   - the settle (`MovieCache.canonicalizeBySanitize`) votes on the STITCHED record — every
 *     cinema slot the film has, read back out of `movie_slots`;
 *   - the fold (`MongoStagingFolder.foldOnce`) plans against RAW `movies` documents
 *     (`StagingFoldIntegrationSpec` pins that read), and a migrated film's `sourceData` is
 *     empty — its cinemas are side rows. So the only cinema titles the fold could see were
 *     the ones on the STAGING rows: whichever venues happened to have diverted.
 *
 * One diverted venue publishing a decorated spelling is therefore an unopposed plurality of
 * one, and the fold re-keyed the whole film onto it. The settle then read the stitched
 * record, saw the venues that publish the film plainly, and re-keyed it back. Neither
 * component is wrong on its own inputs and neither converges: ~83
 * `merges_total{reason="canonicalize"}` a day on the :21/:51 beat, three rating lookups per
 * film per cycle, and the fixpoint leg flipping `Arek. Mama. Panorama` against
 * `Przedpremiera: Arek. Mama. Panorama | Wakacje z dokumentem` on alternate ticks.
 *
 * This is invisible without the split — with `sourceData` embedded, the sibling row carries
 * its twelve plain slots into the fold's pool and the two components agree. That is why the
 * in-memory reproduction (`SettleBeatFixpointSpec`) self-heals in two cycles, and why this
 * spec lives at the `it` layer.
 *
 * Note the difference from `StagingFoldIntegrationSpec`'s "migrated film" case, which probed
 * this same read and concluded the film stays put: it used a SHOUTED variant, which sanitizes
 * to the same key, so no re-key was possible either way. The spellings here sanitize apart,
 * which is the case that moves.
 */
class FoldSpellingAgreesWithSettleSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  // Its own sentinel anchor and tmdbId — see `FoldFixture`, the it suites share one database.
  private val bare      = "__loopaspelling-it-sentinel__"
  private val decorated = s"Przedpremiera: $bare | Wakacje z dokumentem"
  private val tmdbId    = 42432

  // Guard: the two spellings really are distinct keys. If a rule ever collapses them the
  // re-key under test becomes impossible and this spec would pass while asserting nothing.
  require(titleNormalizer.sanitize(bare) != titleNormalizer.sanitize(decorated),
    "the decorated form is now collapsed by a rule — pick another to keep this honest")

  private val bareSanitize      = titleNormalizer.sanitize(bare)
  private val decoratedSanitize = titleNormalizer.sanitize(decorated)

  /** The venues that publish the film plainly — the settled plurality. */
  private val plainVenues = Seq(models.Multikino, models.Helios, models.KinoApollo,
    models.KinoBulgarska, models.CharlieMonroe)
  /** The one venue that dresses it up, and has just diverted into staging. */
  private val fancyVenue  = models.KinoMuza

  it should "keep a film on the spelling its stitched cinemas report, not the one diverted venue's" in {
    FoldFixture.withFold(bareSanitize, decoratedSanitize) { fold =>
      // A fully MIGRATED film, which is what prod's corpus is: the `movies` document carries
      // no `sourceData` at all, and every cinema it has lives in `movie_slots`. Five venues
      // publish it plainly and one dresses it up, so the settled spelling is the plain one.
      val settled = fold.seedMigratedFilm(bare, Some(2026), tmdbId)
      fold.slots.replaceFilm(settled,
        (plainVenues.map(c => c.displayName -> SourceData(title = Some(bare), releaseYear = Some(2026))) :+
          (fancyVenue.displayName -> SourceData(title = Some(decorated), releaseYear = Some(2026)))).toMap)

      // The fancy venue has diverted: its slot is a staging row, concluded on the same film.
      val stagingId = fold.seedStagingRow(fancyVenue.displayName, decorated, Some(2026), tmdbId)

      fold.folder().foldGroup(decorated)

      withClue("the fold consumed no staging row, so it never chose anything: ")(
        fold.stagingRowExists(stagingId) shouldBe false)

      val survivors = fold.filmIds(bareSanitize) ++ fold.filmIds(decoratedSanitize)
      withClue(
        s"survivors=$survivors — the fold could not see the film's five stitched plain " +
        "cinemas (a migrated row reports none) and keyed it on the single diverted venue's " +
        "decorated spelling. The settle reads the stitched record, votes the plain form, and " +
        "re-keys it straight back: that disagreement IS the 30-minute beat.\n") {
        survivors shouldBe Seq(settled)
      }
    }
  }
}
