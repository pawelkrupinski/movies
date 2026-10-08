package services.movies

import models.{Cinema, Country, SourceData}
import tools.{CorpusFixture, FixtureTestWiring}

import java.nio.file.{Files, Path}

/** One venue's listing of a film, as the corpus-shape specs read it: the venue, the title it
 *  published, the slot the pipeline builds of it, and the title the listing's searches start from. */
final case class CorpusListing(cinema: Cinema, raw: String, slot: SourceData, cleanTitle: String)

/** A corpus the shape specs hold to their rules — named, in its country, read through that
 *  country's title rules. `listings` is built once, on first use, so a spec that cancels for an
 *  absent corpus, or one filtered to another country, never pays for it. */
final class ListingCorpus(val label: String, val country: Country, read: TitleNormalizer => Seq[CorpusListing]) {
  /** One normalizer per corpus: each instance compiles the country's rules and fills its own memos. */
  val normalizer: TitleNormalizer = TitleNormalizer.forCountry(country)
  lazy val listings: Seq[CorpusListing] = read(normalizer)
  /** The query a TMDB / IMDb title search for `listing` sends (`MovieService`'s `searchQuery(cleanTitle)`). */
  def query(listing: CorpusListing): String = normalizer.searchQuery(listing.cleanTitle)
}

/**
 * Every corpus a structural corpus spec reads, one per source:
 *
 *  - the Polish fixture boot (`08-06-2026`): each cinema slot of every film the booted pipeline
 *    built, detail-page fields included;
 *  - each country's RECORDED corpus (`cinema-scrapes-<cc>.json.gz`, the recorder's
 *    `scrape-fixtures-<cc>` artifact): what every client's parser handed the pipeline, each listing
 *    built into its slot by the production [[CinemaSlotBuilder]]. Read from
 *    `KINOWO_IDENTITY_CORPUS_DIR` when it holds the country's file, else from
 *    `test/resources/fixtures/corpus` — where Poland's and Germany's are checked in, and where a
 *    convergence leg restores its own country's.
 */
object ListingCorpora {

  val FixtureBootLabel = "fixture boot 08-06-2026"

  def fixtureBoot(wiring: => FixtureTestWiring)(using LatestTitleYear): ListingCorpus =
    new ListingCorpus(FixtureBootLabel, Country.Poland, normalizer =>
      ScheduleCorpusText.recordsByFilmId(wiring).values.toSeq.distinct.flatMap(_.cinemaData.toSeq).map { case (cinema, slot) =>
        val raw = slot.rawTitle.orElse(slot.title).getOrElse("")
        CorpusListing(cinema, raw, slot, normalizer.listingTitle(cinema, raw)._1)
      })

  /** The test-name label of `country`'s recorded corpus — what `-z recorded:<cc>` selects. */
  def recordedLabel(country: Country): String = s"recorded:${country.code}"

  /** Where `country`'s recorded corpus is, if anywhere. */
  def recordedPath(country: Country, corpusDirectory: Option[Path]): Option[Path] =
    (corpusDirectory.map(_.resolve(CorpusFixture.pathFor(country.code).getFileName)).toSeq :+ CorpusFixture.pathFor(country.code))
      .find(Files.exists(_))

  def recorded(country: Country, path: Path): ListingCorpus =
    new ListingCorpus(recordedLabel(country), country, { normalizer =>
      val slots = new CinemaSlotBuilder(country.language, new StringPool)
      CorpusFixture.readFrom(path).flatMap(row => row.films.map(row.cinema -> _)).map { case (cinema, cm) =>
        val clean = ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)._1
        CorpusListing(cinema, cm.movie.rawTitle.getOrElse(cm.movie.title), slots.build(cm, clean, None), clean)
      }
    })

  /** Why a recorded corpus a spec wanted is not here — the cancel message, never a silent pass. */
  def absent(country: Country): String =
    s"corpus not present: no ${CorpusFixture.pathFor(country.code).getFileName} in KINOWO_IDENTITY_CORPUS_DIR or " +
      s"test/resources/fixtures/corpus. Fetch it read-only: gh run download <a 'Record scrape fixtures' run> " +
      s"--name scrape-fixtures-${country.code}, untar it, and point KINOWO_IDENTITY_CORPUS_DIR at the directory holding the file."
}
