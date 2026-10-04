package services.movies

import models.{Helios, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import java.util.concurrent.{ScheduledFuture, ScheduledThreadPoolExecutor, TimeUnit}

/**
 * `StrandedSideRowsCleanup` is the schedule around `MovieRepository.deleteStrandedSideRows`
 * (whose rule `StrandedSideRowsSpec` pins) and the retired-venue sweep: a tick shortly after
 * boot, then daily, the stranded sweep on the ticks it is due (weekly), and a tick that fails
 * must not take the schedule down with it.
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
    screenings.upsertSlot("dead|2020", "helios␟dead", ListedShowtimes(tomorrow, None))
    (repository, screenings)
  }

  "start" should "schedule one sweep shortly after boot and then every 24h, and the tick sweeps" in {
    val (repository, screenings) = repositoryWithStrandedRow()
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(repository, () => RetiredVenueRows.none, () => (), () => true, scheduler)
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
    var retiredSwept = 0
    val failing = new InMemoryMovieRepository(normalizer = titleNormalizer) {
      override def deleteStrandedSideRows(): StrandedSideRows = throw new RuntimeException("mongo went away")
    }
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(failing, () => { retiredSwept += 1; RetiredVenueRows.none }, () => (), () => true, scheduler)
    try {
      cleanup.start()
      noException should be thrownBy scheduler.ticks.head._1.run()
      retiredSwept shouldBe 1   // the stranded sweep throwing does not skip the retired-venue one
    } finally cleanup.stop()
  }

  it should "run the retired-venue sweep on the same tick, and survive it throwing" in {
    val (repository, screenings) = repositoryWithStrandedRow()
    var retiredSwept = 0
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(repository,
      () => { retiredSwept += 1; throw new RuntimeException("mongo went away") }, () => (), () => true, scheduler)
    try {
      cleanup.start()
      noException should be thrownBy scheduler.ticks.head._1.run()
      retiredSwept shouldBe 1
      screenings.findForFilm("dead|2020") shouldBe empty   // the stranded sweep still ran
    } finally cleanup.stop()
  }

  it should "report after BOTH sweeps on every tick, even when they throw" in {
    // The retired-venue census read at boot and then hourly, so each boot's sweep (120s in)
    // showed on the dashboard up to an hour late (prod US 2026-09-23: 13 rows / 221 future
    // showtimes held for 25 min after the sweep had removed 11 of them). The tick now hands
    // the census a fresh reading the moment the sweeps finish.
    val (repository, screenings) = repositoryWithStrandedRow()
    var events = Vector.empty[String]
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(repository,
      () => { events :+= "retired"; throw new RuntimeException("mongo went away") },
      () => events :+= s"reported:${screenings.findForFilm("dead|2020").isEmpty}", () => true, scheduler)
    try {
      cleanup.start()
      noException should be thrownBy scheduler.ticks.head._1.run()
      events shouldBe Vector("retired", "reported:true")
    } finally cleanup.stop()
  }

  "removeStranded" should "return what the repository swept" in {
    val (repository, _) = repositoryWithStrandedRow()
    new StrandedSideRowsCleanup(repository, () => RetiredVenueRows.none, () => (), () => true, new HeldScheduler).removeStranded() shouldBe
      StrandedSideRows(screenings = 1, slots = 0, filmIds = Set("dead|2020"))
  }

  /** The stranded sweep is a backstop now: a film's own delete takes its side rows (the event), so
   *  the sweep runs one day in seven, once that day, while the retired-venue sweep — which no film
   *  delete covers — keeps every daily tick. */
  "the daily tick" should "run the stranded backstop once a week and the retired-venue sweep every day" in {
    val (repository, screenings) = repositoryWithStrandedRow()
    // A Wednesday, the day before the backstop's Thursday.
    val clock     = new _root_.tools.MutableClock(java.time.Instant.parse("2026-10-07T10:00:00Z"))
    val runStore  = new services.schedule.InMemoryScheduledRunStore
    var retired   = 0
    val scheduler = new HeldScheduler
    val cleanup   = new StrandedSideRowsCleanup(repository, () => { retired += 1; RetiredVenueRows.none }, () => (),
      StrandedSideRowsCleanup.weekly(runStore, clock), scheduler)
    try {
      cleanup.start()
      val tick = scheduler.ticks.head._1

      tick.run()                                              // Wednesday: not the backstop's day
      screenings.findForFilm("dead|2020") should not be empty

      clock.advance(java.time.Duration.ofDays(1))             // Thursday
      tick.run()
      screenings.findForFilm("dead|2020") shouldBe empty
      runStore.claimedIds should have size 1

      screenings.upsertSlot("dead|2020", "helios␟dead", ListedShowtimes(tomorrow, None))
      clock.advance(java.time.Duration.ofHours(6))            // a second boot's tick, the same Thursday
      tick.run()
      (1 to 6).foreach { _ => clock.advance(java.time.Duration.ofDays(1)); tick.run() }   // Friday to Wednesday
      screenings.findForFilm("dead|2020") should not be empty

      clock.advance(java.time.Duration.ofDays(1))             // the next Thursday
      tick.run()
      screenings.findForFilm("dead|2020") shouldBe empty
      retired shouldBe 10                                     // every tick swept retired venues
    } finally cleanup.stop()
  }

  /** What the backstop is a backstop FOR: retiring a film (the identity projection's removal of a
   *  merge's loser, or of a film no venue lists) deletes its side rows at that moment, no sweep run. */
  "retiring a film" should "take its side rows with it, without the sweep" in {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots), normalizer = titleNormalizer)
    repository.upsert("Gone", Some(2026), MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Gone"), releaseYear = Some(2026), showtimes = tomorrow))))
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val id    = cache.idOf(cache.keyOf("Gone", Some(2026))).get
    screenings.findForFilm(id.value) should not be empty
    slots.findForFilm(id.value) should not be empty

    cache.retireProjected(id) shouldBe WriteOutcome.Written

    screenings.findForFilm(id.value) shouldBe empty
    slots.findForFilm(id.value) shouldBe empty
  }
}
