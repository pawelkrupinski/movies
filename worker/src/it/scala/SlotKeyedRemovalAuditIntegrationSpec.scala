package services

import ch.qos.logback.classic.Level
import models.{Showtime, SourceData}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListedShowtimes, MongoScreeningsRepository, MongoSlotsRepository, RemovalAudit, SlotKeyedRows}
import _root_.tools.{IsolatedMongoDatabase, LogCapture}

import java.time.LocalDateTime

/**
 * Every row the two Mongo side stores remove is told to the removal audit, under the collection's name: a slot deleted,
 * a film's rows cleared, and the stale slots a `replaceFilm` prunes -- the lines a "where did this film's showtimes go"
 * investigation reads (`kinowo.removal-audit`).
 */
class SlotKeyedRemovalAuditIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with _root_.tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "slot-keyed-removal-audit")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def audited(body: => Unit): Seq[String] =
    LogCapture.thisThread(RemovalAudit.LoggerName, Some(Level.DEBUG))(body).map(_.getFormattedMessage)

  private val showtime = Showtime(LocalDateTime.of(2026, 10, 1, 19, 15), Some("https://book"), Some("1"), List("2D"))

  /** A store, how it writes one film's two venues, and how it keeps only the first. */
  private final case class Store(rows: SlotKeyedRows, writeBoth: String => Unit, keepHelios: String => Unit)

  private lazy val screenings = {
    val store = new MongoScreeningsRepository(Some(isolatedDatabase.database), clock = _root_.tools.SpecClock.Pinned, stopwatch = _root_.tools.Stopwatch.System)
    val row   = ListedShowtimes(Seq(showtime), None)
    Store(store, film => { store.replaceFilm(film, Map("Helios" -> row, "Multikino" -> row)); () },
      film => { store.replaceFilm(film, Map("Helios" -> row)); () })
  }

  private lazy val slots = {
    val store = new MongoSlotsRepository(Some(isolatedDatabase.database), clock = _root_.tools.SpecClock.Pinned, stopwatch = _root_.tools.Stopwatch.System)
    val slot  = SourceData(title = Some("Film"), rawTitle = Some("Film"), showtimes = Seq(showtime))
    Store(store, film => { store.replaceFilm(film, Map("Helios" -> slot, "Multikino" -> slot)); () },
      film => { store.replaceFilm(film, Map("Helios" -> slot)); () })
  }

  Seq(("screenings", "screenings", () => screenings), ("movie_slots", "slots", () => slots)).foreach { case (collection, what, store) =>
    s"the $collection store" should "audit a slot it deletes, under its collection" in {
      val s = store()
      s.writeBoth("deleted-slot|2026")
      audited(s.rows.deleteSlot("deleted-slot|2026", "Multikino")) shouldBe
        Seq(s"[$collection.deleteSlot] slot removed: filmId=deleted-slot|2026 slot='Multikino' reason=slot-deleted")
    }

    it should "audit a whole film it clears, counting its rows" in {
      val s = store()
      s.writeBoth("deleted-film|2026")
      audited(s.rows.deleteFilm("deleted-film|2026")) shouldBe
        Seq(s"[$collection.deleteFilm] $what CLEARED: filmId=deleted-film|2026 slots=2 reason=film-deleted")
      audited(s.rows.deleteFilm("deleted-film|2026")) shouldBe empty
    }

    it should "audit the stale slots a replaceFilm prunes, and nothing when none is stale" in {
      val s = store()
      s.writeBoth("pruned|2026")
      audited(s.keepHelios("pruned|2026")) shouldBe
        Seq(s"[$collection.replaceFilm] $what trimmed: filmId=pruned|2026 slots=1 reason=stale-slot-prune")
      audited(s.keepHelios("pruned|2026")) shouldBe empty
    }
  }
}
