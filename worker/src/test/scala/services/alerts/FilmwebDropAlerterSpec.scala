package services.alerts

import org.scalatest.matchers.should.Matchers
import models.{Cinema, KinoTatry, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.ScrapeOutcome
import services.cinemas.common.ScrapeOutcome._

import scala.collection.mutable.ListBuffer

/**
 * FilmwebDropAlerter: a Filmweb-only venue that has served data, then goes
 * empty/failing for `threshold` consecutive scrapes, fires exactly one alert
 * until it recovers. A venue that never served data this lifetime (genuinely
 * dormant) and any non-Filmweb cinema are ignored.
 */
class FilmwebDropAlerterSpec extends AnyFlatSpec with Matchers {

  // KinoTatry is a real Filmweb venue; Multikino is a chain (not in the watch set).
  private val watched = Set(KinoTatry.displayName)

  private def newAlerter(threshold: Int = 3): (FilmwebDropAlerter, ListBuffer[String]) = {
    val sent = ListBuffer.empty[String]
    val record: String => Unit = s => { sent += s; () }
    (new FilmwebDropAlerter(watched, record, threshold), sent)
  }

  private def feed(a: FilmwebDropAlerter, c: Cinema, outcomes: ScrapeOutcome*): Unit =
    outcomes.foreach(o => a.onOutcome(c, o))

  "FilmwebDropAlerter" should "alert once after threshold consecutive empties following a success" in {
    val (a, sent) = newAlerter()
    feed(a, KinoTatry, Success, Empty, Empty)   // only 2 empties — not yet
    sent shouldBe empty
    feed(a, KinoTatry, Empty)                   // 3rd consecutive → alert
    sent.size shouldBe 1
    sent.head should include ("Kino Tatry")
    sent.head should include ("Filmweb")
    feed(a, KinoTatry, Empty, Empty)            // still dropped → no repeat
    sent.size shouldBe 1
  }

  it should "count failures the same as empties toward the threshold" in {
    val (a, sent) = newAlerter()
    feed(a, KinoTatry, Success, Empty, Failure, Empty)   // 3 mixed down in a row
    sent.size shouldBe 1
  }

  it should "NOT alert a venue that never served data this lifetime (genuinely dormant)" in {
    val (a, sent) = newAlerter()
    feed(a, KinoTatry, Empty, Empty, Empty, Empty, Failure)   // no success first
    sent shouldBe empty
  }

  it should "reset the down-streak on a success between bad scrapes" in {
    val (a, sent) = newAlerter()
    feed(a, KinoTatry, Success, Empty, Empty, Success, Empty, Empty)   // never 3 in a row
    sent shouldBe empty
  }

  it should "re-alert after the venue recovers and drops again" in {
    val (a, sent) = newAlerter()
    feed(a, KinoTatry, Success, Empty, Empty, Empty)   // alert #1
    feed(a, KinoTatry, Success)                        // recovered → clears
    feed(a, KinoTatry, Empty, Empty, Empty)            // alert #2
    sent.size shouldBe 2
  }

  it should "ignore cinemas that aren't in the Filmweb watch set" in {
    val (a, sent) = newAlerter()
    feed(a, Multikino, Success, Empty, Empty, Empty, Empty)
    sent shouldBe empty
  }

  it should "honour a custom threshold" in {
    val (a, sent) = newAlerter(threshold = 1)
    feed(a, KinoTatry, Success, Empty)   // one empty meets threshold 1
    sent.size shouldBe 1
  }
}
