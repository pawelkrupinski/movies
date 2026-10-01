package services.identity

import models.Cinema
import org.scalatest.Assertions.fail
import services.scrapes.{ArchivedScrape, BarrenAttempt, ContentStamp, ScrapeArchiveRepository, SuccessfulScrape}

/** A read-only archive that serves `rows` in pages of `pageSize` and refuses to hand over the whole
 *  archive at once — the shape a reader that must stream (the shadow's and the cutover's listing
 *  sets) is held to. `completes = false` fails the read after its first page. */
final class PagedScrapeArchive(rows: Seq[ArchivedScrape], pageSize: Int, completes: Boolean = true) extends ScrapeArchiveRepository {
  var pagesServed = 0
  def enabled: Boolean = true
  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = ()
  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit     = ()
  def find(cinema: Cinema): Option[ArchivedScrape]  = rows.find(_.cinema == cinema)
  def contentStamps(): Map[String, ContentStamp]    = Map.empty
  override def findAll(): Seq[ArchivedScrape]       = fail("the whole archive was asked for at once")
  def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = {
    val pages = rows.grouped(pageSize).toSeq
    pages.take(if (completes) pages.size else 1).foreach { page => pagesServed += 1; consume(page) }
    completes
  }
}
