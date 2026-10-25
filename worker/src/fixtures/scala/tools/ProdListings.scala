package tools

import services.identity.IdentityListingIntake
import services.scrapes.{ArchivedScrape, MongoScrapeArchiveRepository}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.util.Using

/**
 * The listings prod's identity model reads: each venue's ACCEPTED listing (`identity_listings`, the scrape-health guards'
 * pick — `IdentityListingIntake`), and the scrape archive's last listing only for a venue with none accepted. A recording's
 * corpus is the archive's (`cinema_scrapes`): a scrape the guards held back is in it and not in the model, which still
 * resolves the listing it accepted before. Captured beside the corpus (`RecordCorpusFixture`, the same connection), so the
 * live drift check (`scripts.IdentityLiveDrift`) resolves what the model resolved ([[merged]]).
 */
object ProdListings {

  def path(root: Path, code: String): Path = root.resolve("corpus").resolve(s"prod-listings-$code.json.gz")

  /** Every accepted listing in `database` (prod's country database, read-only) of the `known` venues — one that lists
   *  nothing included: the model reads it as the venue listing nothing, not the archive's. */
  def read(database: org.mongodb.scala.MongoDatabase, known: Set[models.Cinema]): Seq[ArchivedScrape] =
    new MongoScrapeArchiveRepository(Some(database), IdentityListingIntake.Collection).findAll().filter(row => known.contains(row.cinema))

  /** `scrapes` as the model reads them: each venue's accepted row in place of its scrape, a venue only `accepted` holds
   *  added; a venue listing nothing left out, as the recorder leaves it out. */
  def merged(scrapes: Seq[ArchivedScrape], accepted: Seq[ArchivedScrape]): Seq[ArchivedScrape] = {
    val (byVenue, scraped) = (accepted.map(row => row.cinema -> row).toMap, scrapes.map(_.cinema).toSet)
    (scrapes.map(row => byVenue.getOrElse(row.cinema, row)) ++ accepted.filterNot(row => scraped(row.cinema)))
      .filter(_.films.nonEmpty)
  }

  def write(root: Path, code: String, rows: Seq[ArchivedScrape]): Path = {
    val file = path(root, code)
    Files.createDirectories(file.getParent)
    Using.resource(new GZIPOutputStream(Files.newOutputStream(file)))(_.write(CorpusFixture.render(rows).getBytes(StandardCharsets.UTF_8)))
    file
  }

  /** What a recording laid out at `root` captured of prod's accepted listings; None for one recorded before it did. */
  def readFrom(root: Path, code: String): Option[Seq[ArchivedScrape]] = Option(path(root, code)).filter(Files.exists(_)).map(CorpusFixture.readFrom)
}
