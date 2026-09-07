package services.movies

import models.{Helios, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import java.util.concurrent.{ScheduledFuture, ScheduledThreadPoolExecutor, TimeUnit}

/**
 * `StrandedSideRowsCleanup` is the schedule around `MovieRepository.deleteStrandedSideRows`
 * (whose rule `StrandedSideRowsSpec` pins): one run shortly after boot, then daily, and
 * a tick that fails must not take the schedule down with it.
 */
class StrandedSideRowsCleanupSpec extends AnyFlatSpec with Matchers {

  /** Records what was scheduled instead of running it, so a spec can hold the tick and
   *  fire it by hand. Built on the real pool only to honour the interface. */
  private class HeldScheduler extends ScheduledThreadPoolExecutor(1) {
    var ticks = Vector.empty[(Runnable, Long, Long, TimeUnit)]
    override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[?] = {
      ticks :+= ((command, initialDelay, period, unit))
      super.schedule((() => ()): Runnable, 0, unit)
    }
  }

  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1), bookingUrl = None))

  private def repositoryWithStrandedRow() = {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
    repository.upsert("Live", Some(2026), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Live"), releaseYear = Some(2026), showtimes = tomorrow))))
    screenings.upsertSlot("dead|2020", "helios␟dead", tomorrow)
    (repository, screenings)
  }

  "start" should "schedule one sweep shortly after boot and then every 24h, and the tick sweeps" in {
    val (repository, screenings) = repositoryWithStrandedRow()
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(repository, scheduler)
    try {
      cleanup.start()

      scheduler.ticks should have size 1
      val (tick, initialDelay, period, unit) = scheduler.ticks.head
      unit.toSeconds(initialDelay) should (be > 0L and be <= 600L)
      unit.toHours(period) shouldBe 24L

      screenings.findForFilm("dead|2020") should not be empty
      tick.run()
      screenings.findForFilm("dead|2020") shouldBe empty
    } finally cleanup.stop()
  }

  it should "survive a tick whose sweep throws" in {
    val failing = new InMemoryMovieRepository(normalizer = titleNormalizer) {
      override def deleteStrandedSideRows(): StrandedSideRows = throw new RuntimeException("mongo went away")
    }
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(failing, scheduler)
    try {
      cleanup.start()
      noException should be thrownBy scheduler.ticks.head._1.run()
    } finally cleanup.stop()
  }

  "removeStranded" should "return what the repository swept" in {
    val (repository, _) = repositoryWithStrandedRow()
    new StrandedSideRowsCleanup(repository, new HeldScheduler).removeStranded() shouldBe
      StrandedSideRows(screenings = 1, slots = 0, filmIds = Set("dead|2020"))
  }
}
