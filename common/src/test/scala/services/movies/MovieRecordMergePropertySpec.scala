package services.movies

import models.{Helios, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators._

/**
 * The invariants `MovieRecordMerge` documents, stated over generated rows.
 *
 * `MovieRecord` equality excludes showtimes by design (a cache-resident slot is
 * stripped to `Nil`), so the merge's showtime handling is asserted separately.
 */
class MovieRecordMergePropertySpec extends IdentityPropertySpec {

  private def showtimesOf(record: MovieRecord): Map[Source, Seq[Showtime]] =
    record.data.view.mapValues(_.showtimes).toMap

  /** A pool small enough that several rows hold a slot under the SAME source —
   *  two venues, two spellings, plus the derived sources every resolved row carries. */
  private val genCrowdedRecord: Gen[MovieRecord] =
    genMovieRecord(Gen.frequency(
      2 -> genCinemaSlot(cinemaPool = Seq(Multikino, Helios), titles = Gen.oneOf("Diuna", "Diuna 2")),
      1 -> genEnrichmentSlot))

  "MovieRecordMerge.union" should "be idempotent: folding a row onto itself changes nothing" in {
    forAll(genMovieRecord()) { a =>
      val merged = MovieRecordMerge.union(a, a)
      merged shouldBe a
      showtimesOf(merged) shouldBe showtimesOf(a)
    }
  }

  it should "merge the per-source data commutatively, as `mergeSlot` documents" in {
    forAll(genCrowdedRecord, genCrowdedRecord) { (a, b) =>
      val ab = MovieRecordMerge.union(a, b)
      val ba = MovieRecordMerge.union(b, a)
      ab.data shouldBe ba.data
      showtimesOf(ab) shouldBe showtimesOf(ba)
      ab.retainedSynopses shouldBe ba.retainedSynopses
    }
  }

  // ── unionAll is NOT order-free once three rows hold a slot under one source ──
  //
  // `canonical` documents the fold as "order independent for the per-source
  // `data` (it's a keyed merge)", and `mergeSlot` is commutative — but it is not
  // ASSOCIATIVE. `richer` settles a disagreement on the side with MORE populated
  // fields, and a merged slot is richer than either input, so which of two
  // disagreeing values survives depends on whether the third slot was folded in
  // before or after the disagreement was settled. Two rows can never see it; the
  // third row does. `FilmCanonicalizer.canonical` sorts the cluster (`mergeOrder`)
  // before folding, so the settle is deterministic anyway; the cache's rehydrate
  // fold is the caller that hands `unionAll` rows in read order.
  //
  // Both specs below found `unionAll` order-DEPENDENT when written: the pairwise
  // slot fold was commutative but not associative. `mergeSlots` now settles every
  // slot of a source at once, richest first, so they hold.

  "MovieRecordMerge.unionAll" should "settle a three-way slot disagreement the same way in any order (MINIMAL counterexample)" in {
    // Three rows of one film, each with a Helios slot. `a` and `c` disagree on
    // the title; `b` says nothing about it but is populated enough to tip the
    // richness count of whatever it merges into.
    val a = SourceData(title = Some("Diuna"), cast = Seq("Timothée Chalamet"))                              // 2 fields
    val b = SourceData(director = Seq("Denis Villeneuve"), genres = Seq("Sci-Fi"))                        // 2 fields
    val c = SourceData(title = Some("Dune"), cast = Seq("Zendaya"), countries = Seq("USA"))                // 3 fields
    def row(slot: SourceData): MovieRecord = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Helios -> slot))

    // (a ⊕ b) has 4 fields and beats c, so "Diuna" survives; (a ⊕ c) is settled
    // on c's richer slot first, so "Dune" survives and b cannot undo it.
    MovieRecordMerge.unionAll(Seq(row(a), row(b), row(c))).data(Helios).title shouldBe
      MovieRecordMerge.unionAll(Seq(row(a), row(c), row(b))).data(Helios).title

  }

  it should "merge the per-source data the same way whatever order the rows arrive in" in {
    // One venue, one title, plus the derived sources: every row's slots collide.
    // The single-source fields are NOT claimed order-free (the first
    // tmdbId-bearing row is the base), so only `data` and the retained synopses
    // are compared. Enough cases that the three-way shape above is always drawn.
    val genCollidingRecord: Gen[MovieRecord] =
      genMovieRecord(Gen.frequency(
        2 -> genCinemaSlot(cinemaPool = Seq(Helios), titles = Gen.const("Diuna")),
        1 -> genEnrichmentSlot), maxSlots = 2)
    val genResolvedRows: Gen[Seq[MovieRecord]] = for {
      n    <- Gen.choose(3, 4)
      rows <- Gen.listOfN(n, genCollidingRecord)
    } yield rows.head.copy(tmdbId = Some(1)) +: rows.tail

    forAll(withPermutation(genResolvedRows), minSuccessful(1000)) { case (rows, permuted) =>
      val expected = MovieRecordMerge.unionAll(rows)
      val actual   = MovieRecordMerge.unionAll(permuted)
      actual.data shouldBe expected.data
      showtimesOf(actual) shouldBe showtimesOf(expected)
      actual.retainedSynopses shouldBe expected.retainedSynopses
    }

  }
}
