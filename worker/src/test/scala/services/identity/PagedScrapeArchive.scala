package services.identity

import models.Cinema
import org.scalatest.Assertions.fail
import services.scrapes.{ArchivedScrape, BarrenAttempt, ContentStamp, ScrapeArchiveRepository, SuccessfulScrape}

/** A read-only archive that serves `rows` in pages of `pageSize` and refuses to hand over the whole
 *  archive at once — the shape a reader that must stream (the shadow's and the cutover's listing
 *  sets) is held to. `completes = false` fails the read after its first page. Like the Mongo
 *  archive, a [[scanVenues]] leaves the venues it does not keep out before paging, never serving
 *  their rows; `venuesServed` is every row's venue that was. */
final class PagedScrapeArchive(rows: Seq[ArchivedScrape], pageSize: Int, completes: Boolean = true) extends ScrapeArchiveRepository {
  var pagesServed  = 0
  var venuesServed = Vector.empty[Cinema]
  def enabled: Boolean = true
  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = ()
  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit     = ()
  def find(cinema: Cinema): Option[ArchivedScrape]  = rows.find(_.cinema == cinema)
  def contentStamps(): Map[String, ContentStamp]    = Map.empty
  override def findAll(): Seq[ArchivedScrape]       = fail("the whole archive was asked for at once")
  def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = scanVenues(_ => true)(consume)
  override def scanVenues(keep: Cinema => Boolean)(consume: Seq[ArchivedScrape] => Unit): Boolean = {
    val pages = rows.filter(row => keep(row.cinema)).grouped(pageSize).toSeq
    pages.take(if (completes) pages.size else 1).foreach { page =>
      pagesServed += 1
      venuesServed ++= page.map(_.cinema)
      consume(page)
    }
    completes
  }
}
