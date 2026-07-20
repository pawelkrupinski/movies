package services.movies

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.core.read.ListAppender
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

/** Locks the removal-audit contract: every helper writes to the ONE dedicated
 *  `kinowo.removal-audit` logger, at the level the diagnostic policy demands
 *  (whole-film / batch / guard-skip = INFO, routine single-slot churn = DEBUG),
 *  with the film id and a reason in the line. This is the fail-before/pass-after
 *  gate for the audit: before it existed the removal sites logged nothing. */
class RemovalAuditSpec extends AnyFlatSpec with Matchers {

  /** Run `body` with a logback ListAppender attached to the audit logger (forced to
   *  DEBUG so both levels are captured), returning the events it emitted. */
  private def capture(body: => Unit): Seq[ILoggingEvent] = {
    val lg  = LoggerFactory.getLogger(RemovalAudit.LoggerName).asInstanceOf[LogbackLogger]
    val app = new ListAppender[ILoggingEvent]()
    app.start()
    val prevLevel = lg.getLevel
    lg.setLevel(Level.DEBUG)
    lg.addAppender(app)
    try { body; app.list.asScala.toSeq }
    finally { lg.detachAppender(app); lg.setLevel(prevLevel) }
  }

  "RemovalAudit.filmRemoved" should "log one INFO line carrying the id and reason" in {
    val events = capture(RemovalAudit.filmRemoved("movies.delete", "odyseja|2026", "title+year"))
    events.map(_.getLevel) shouldBe Seq(Level.INFO)
    val msg = events.head.getFormattedMessage
    msg should include ("odyseja|2026")
    msg should include ("title+year")
    msg should include ("movies.delete")
  }

  "RemovalAudit.filmsRemoved" should "log an INFO batch line with the count and a capped id sample" in {
    val ids    = (1 to 25).map(i => s"film-$i")
    val events = capture(RemovalAudit.filmsRemoved("unscreened-cleanup", ids, "no-current-screenings"))
    events.map(_.getLevel) shouldBe Seq(Level.INFO)
    val msg = events.head.getFormattedMessage
    msg should include ("25 film(s)")
    msg should include ("(+5 more)")            // 25 ids, cap 20
    msg should include ("no-current-screenings")
  }

  it should "stay silent on an empty batch" in {
    capture(RemovalAudit.filmsRemoved("unscreened-cleanup", Nil, "no-current-screenings")) shouldBe empty
  }

  "RemovalAudit.scrapePruned" should "log INFO when it dropped slots and stay silent otherwise" in {
    capture(RemovalAudit.scrapePruned("Multikino Rybnik", films = 0, slots = 0, Nil, "scrape-prune")) shouldBe empty
    val events = capture(RemovalAudit.scrapePruned("Multikino Rybnik", films = 3, slots = 5,
      Seq("A (2026)", "B (2026)"), "scrape-prune"))
    events.map(_.getLevel) shouldBe Seq(Level.INFO)
    val msg = events.head.getFormattedMessage
    msg should include ("Multikino Rybnik")
    msg should include ("5 slot(s)")
    msg should include ("3 film(s)")
  }

  "RemovalAudit.scrapePruneSkipped" should "log the guard decision at INFO" in {
    val events = capture(RemovalAudit.scrapePruneSkipped("Multikino Kraków", batchFilms = 3, knownSlots = 45, "partial-scrape-guard"))
    events.map(_.getLevel) shouldBe Seq(Level.INFO)
    val msg = events.head.getFormattedMessage
    msg should include ("SKIPPED")
    msg should include ("Multikino Kraków")
    msg should include ("known=45")
  }

  "RemovalAudit.screeningsCleared" should "log INFO for a whole-film clear and DEBUG for a partial trim" in {
    capture(RemovalAudit.screeningsCleared("screenings.replaceFilm", "odyseja|2026", 4, whole = true, "stale-slot-prune"))
      .map(_.getLevel) shouldBe Seq(Level.INFO)
    capture(RemovalAudit.screeningsCleared("screenings.replaceFilm", "odyseja|2026", 1, whole = false, "stale-slot-prune"))
      .map(_.getLevel) shouldBe Seq(Level.DEBUG)
  }

  "RemovalAudit.slotRemoved" should "log routine single-slot churn at DEBUG" in {
    capture(RemovalAudit.slotRemoved("screenings.deleteSlot", "odyseja|2026", "Kino Hel␟odyseja", "slot-deleted"))
      .map(_.getLevel) shouldBe Seq(Level.DEBUG)
  }
}
