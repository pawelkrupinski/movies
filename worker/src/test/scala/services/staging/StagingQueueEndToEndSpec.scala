package services.staging

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Cinema, Helios, MovieRecord}
import services.movies.CacheKey
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tools.costs.{CostScaling, Work}

/**
 * End-to-end: a newcomer in `pending_movies` is driven through the durable queue
 * — StagingDetail → StagingResolveTmdb → StagingResolveImdbId → StagingFold — by
 * the real handlers + `StagingReaper`, and lands in `movies` — wired and pumped by
 * [[StagingChain]], which stands in for the prod `TaskWorker`.
 */
class StagingQueueEndToEndSpec extends AnyFlatSpec with Matchers {
  import StagingChain.{CountingEnricher, listing}

  "the queue-driven staging chain" should "incubate a newcomer all the way into movies" in {
    // A deferred-detail cinema (Helios) scrapes the film bare with a filmUrl; its
    // detail page supplies a director hint; TMDB resolves to a tmdbId but ships no
    // imdb cross-reference, so the imdb step must recover it.
    val chain = new StagingChain(new InMemoryStagingRepository, Seq(new CountingEnricher(Helios)))
    chain.staging.upsert(Helios, "Newcomer", Some(2026), listing(Helios, "Newcomer"))

    // Kick the chain, then drain — completing a step announces it, which enqueues the
    // next step for this same claim-loop.
    chain.reaper.tick()
    chain.pump()

    val folded = chain.movies.findAll()
    folded.map(_.title) shouldBe Seq("Newcomer")
    folded.head.record.tmdbId shouldBe Some(1275779)
    folded.head.record.imdbId shouldBe Some("tt1275779")
    folded.head.record.director should contain("Jane Doe")           // detail hint carried through the fold
    chain.staging.findAll() shouldBe empty                            // graduated out of pending_movies
    // Wired the production way (see WorkerWiring): the fold announces the brand-new
    // films it introduced, which production turns into rating enqueues.
    chain.promoted shouldBe Seq(CacheKey("Newcomer", Some(2026), titleNormalizer))
  }

  /**
   * A film's staging group is read a number of times LINEAR in its detail venues.
   *
   * THE BUG THIS PINS. Every venue's finished detail step made the reaper re-read the
   * film's WHOLE group to learn whether any venue still owed detail — one decode of every
   * venue's row per venue, O(venues²): 52,000 rows for a film at 160 detail venues. Only
   * the finish that leaves no venue owing advances the film, and which venues owe is
   * answerable off the repository's index (`cinemasUnder`), so only that finish reads
   * the group. Counted as rows the group reads return — Mongo's decodes — not timed.
   */
  it should "read a film's staging group linearly in its detail venues, not once per venue" in {
    def rowsDecoded(venueCount: Int): Long = {
      val work    = new Work
      val staging = Work.counting(classOf[StagingRepository], new InMemoryStagingRepository, work, Work.StagingIndexReads)
      val venues  = Cinema.all.distinct.take(venueCount)
      val chain   = new StagingChain(staging, venues.map(new CountingEnricher(_)))
      venues.foreach(v => staging.upsert(v, "Newcomer", Some(2026), listing(v, "Newcomer")))
      chain.reaper.tick()
      chain.pump(limit = 10 * venueCount)
      // It must actually have folded — a chain that stalled would read little and pass.
      withClue("the film must have graduated: ") { chain.movies.findAll().map(_.title) shouldBe Seq("Newcomer") }
      work.reads
    }
    CostScaling.assertLinear("staging rows read carrying one film through N detail venues — a group read per " +
      "finished detail step is O(venues²)", n = 40, perUnit = 12.0)(rowsDecoded)
  }

  /**
   * Reading the group only on the LAST detail finish must not change what the chain
   * does: a venue that JOINS mid-chain (a join does not kick the chain) still has its
   * detail fetched, without waiting for the backstop tick, and the film folds exactly as
   * it does when every venue staged it before the kick.
   */
  it should "fetch a mid-chain joiner's detail and fold exactly as if it had been there from the kick" in {
    val venues = Cinema.all.distinct.take(7)
    def run(joinAfter: Option[Int]): (Seq[(String, Option[Int], MovieRecord)], Map[Cinema, Int]) = {
      val enrichers = venues.map(new CountingEnricher(_))
      val chain     = new StagingChain(new InMemoryStagingRepository, enrichers)
      val (initial, joiner) = if (joinAfter.isDefined) (venues.init, venues.lastOption) else (venues, None)
      initial.foreach(v => chain.staging.upsert(v, "Newcomer", Some(2026), listing(v, "Newcomer")))
      chain.reaper.tick()
      joinAfter.foreach { n =>
        chain.pump(limit = n)
        joiner.foreach(v => chain.staging.upsert(v, "Newcomer", Some(2026), listing(v, "Newcomer")))
      }
      chain.pump(limit = 200)
      (chain.movies.findAll().map(r => (r.title, r.year, r.record)), enrichers.map(e => e.cinema -> e.fetches).toMap)
    }
    val (reference, _)      = run(joinAfter = None)
    val (joined, fetches)   = run(joinAfter = Some(2))
    withClue("every venue's detail fetched once, the joiner's included: ") { fetches shouldBe venues.map(_ -> 1).toMap }
    withClue("and the fold is the one a from-the-kick venue set produces: ") {
      joined shouldBe reference
      joined.map(_._1) shouldBe Seq("Newcomer")
    }
  }
}
