package services.staging

import models._
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.util.Locale

/**
 * `StagingFold.planGroup`'s contract when two GENUINELY different films (different
 * tmdbId, different imdbId — `clusterByFilm` keeps them in separate clusters) are
 * forced to conclude the identical `(sanitize(title), year)` key: the exact shape of
 * the 2026-09-08 `key_1` incident (see the "Lalka" test in `StagingFoldSpec`) and of
 * the round-1/3/4 fixes that were built for it and then deliberately reverted, keeping
 * only the round-2 tmdbId-race retry.
 *
 * The CURRENT code has NO cross-cluster collision avoidance — that is a deliberate,
 * accepted trade-off (see `nextAfterAttempt`'s doc comment in `StagingFold.scala`), not
 * an oversight this spec exists to flag. So this spec does NOT assert
 * "`moviesUpserts` never repeats a `CacheKey`" — it deliberately DOES, every generated
 * case. What it asserts is the narrower set of invariants the current code is actually
 * meant to guarantee even while accepting that trade-off:
 *
 *   - `planGroup` never throws, whatever the input.
 *   - The collision happens EXACTLY the documented way (two upserts, one key) — a
 *     property test making that trade-off itself visible and regression-proof, so a
 *     future change that starts silently dropping or merging one side is caught here.
 *   - No single upserted `MovieRecord` ever mixes cinema data from BOTH films — the
 *     invariant the first (reverted) fix attempt violated by merging.
 *   - The two colliding films get DISTINCT ids — the bug this very property spec
 *     found live in the pre-existing code (see `StagingFoldSpec`'s "mint DISTINCT ids
 *     for two brand-new films colliding on the same key"): `FilmId.fresh` is a pure
 *     function of the key alone, so two brand-new clusters concluding one key used to
 *     mint the SAME id, and `MongoStagingFolder`'s second `replaceOne` then silently
 *     overwrote the first film's document instead of hitting `key_1` at all.
 *   - The plan is independent of which cluster's staging rows arrive first — the
 *     specific determinism property that took three reverted rounds to get right.
 */
class StagingFoldCollisionPropertySpec extends IdentityPropertySpec {

  // Two DISJOINT cinema pools and DISJOINT tmdbId/imdbId pools, so identity A and
  // identity B are two different films BY CONSTRUCTION — the generator's only job is
  // to force them onto the identical key, not to fake up `clusterByFilm`'s own
  // same-tmdbId/imdbId grouping rules.
  private val cinemasA: Seq[Cinema] = Seq(Multikino, Helios)
  private val cinemasB: Seq[Cinema] = Seq(KinoApollo, KinoMuza)
  private val tmdbIdsA: Seq[Int]    = Seq(1001, 1002, 1003)
  private val tmdbIdsB: Seq[Int]    = Seq(2001, 2002, 2003)
  private val imdbIdsA: Gen[Option[String]] = Gen.oneOf(None, Some("ttA1"), Some("ttA2"))
  private val imdbIdsB: Gen[Option[String]] = Gen.oneOf(None, Some("ttB1"), Some("ttB2"))

  private val baseTitles = Seq("Diuna", "Zaplątani", "Lalka", "Obcy")

  // Spellings that all SANITIZE identically (case, punctuation, whitespace) — the
  // "two different display titles, same key" shape. `CacheKey.equals` compares on
  // `sanitize` + year, not the raw spelling, so identity A and identity B need not
  // agree on which spelling they use, only on which sanitize form it collapses to.
  private def spellings(base: String): Gen[String] =
    Gen.oneOf(base, base.toUpperCase(Locale.ROOT), s"$base.", s" $base ")

  private case class Identity(tmdbId: Int, imdbId: Option[String], tmdbYear: Int, cinemaYear: Int, rows: Seq[(Cinema, String)])

  // `cinemaYear` is fixed PER IDENTITY (not per row) and deliberately kept OFF the
  // shared `tmdbYear` — two cinema-reported rows sharing a raw (sanitize, year) key
  // union BEFORE `groupByFilm`/`clusterByFilm` ever run (`planGroup`'s `stagingByKey`
  // step), so if identity A's and identity B's OWN cinema rows happened to share a raw
  // key too, `MovieRecordMerge.unionAll` would glue them into one row and blur which
  // film a cinema's slot came from — a confound this spec does not want to test.
  // Keeping every raw cinema-reported year OFF the shared final (TMDB) year sidesteps
  // that entirely: the two identities' rows never coincide on a raw key, and the
  // FINAL canonical year (driven by `clusterYear`'s tmdbYear preference) is still
  // forced identical via the shared `tmdbYear` on each identity's own `Tmdb` slot.
  private def genIdentity(base: String, tmdbYear: Int, cinemaYear: Int, cinemas: Seq[Cinema],
                           tmdbIds: Seq[Int], imdbIds: Gen[Option[String]]): Gen[Identity] = for {
    tmdbId  <- Gen.oneOf(tmdbIds)
    imdbId  <- imdbIds
    n       <- Gen.choose(1, cinemas.size)
    chosen  <- Gen.pick(n, cinemas)
    titles  <- Gen.listOfN(chosen.size, spellings(base))
  } yield Identity(tmdbId, imdbId, tmdbYear, cinemaYear, chosen.toSeq.zip(titles))

  /** Two DIFFERENT films forced to conclude the identical `(sanitize(title), year)`
   *  key: a shared base title (allowing different but same-sanitizing spellings per
   *  cinema) and a shared TMDB year, but genuinely different identities. */
  private val genCollidingPair: Gen[(Identity, Identity)] = for {
    base <- Gen.oneOf(baseTitles)
    year <- Gen.oneOf(2024, 2025, 2026)
    a    <- genIdentity(base, year, cinemaYear = year - 1, cinemasA, tmdbIdsA, imdbIdsA)
    b    <- genIdentity(base, year, cinemaYear = year + 1, cinemasB, tmdbIdsB, imdbIdsB)
  } yield (a, b)

  private def stagingRowsOf(identity: Identity): Seq[StagingRecord] =
    identity.rows.map { case (cinema, title) =>
      StagingRecord(cinema, title, Some(identity.cinemaYear), MovieRecord(
        tmdbId = Some(identity.tmdbId), imdbId = identity.imdbId,
        data = Map[Source, SourceData](
          cinema -> SourceData(title = Some(title), releaseYear = Some(identity.cinemaYear)),
          Tmdb   -> SourceData(title = Some(title), releaseYear = Some(identity.tmdbYear)))), titleNormalizer)
    }

  private def cinemasOf(identity: Identity): Set[Source] = identity.rows.map { case (c, _) => c: Source }.toSet

  private def plan(a: Identity, b: Identity): StagingFold.Plan =
    StagingFold.planGroup(stagingRowsOf(a) ++ stagingRowsOf(b), moviesRows = Seq.empty, titleNormalizer)

  "planGroup, fed two different films forced onto one key" should "never throw" in {
    forAll(genCollidingPair) { case (a, b) =>
      noException should be thrownBy plan(a, b)
    }
  }

  it should "actually collide onto one key, undeferred — the accepted trade-off, not a bug this spec is flagging" in {
    forAll(genCollidingPair) { case (a, b) =>
      val p = plan(a, b)
      withClue(s"expected both colliding clusters planned, neither deferred: ${p.moviesUpserts}\n") {
        p.moviesUpserts should have size 2
      }
      p.moviesUpserts.map(_._2).toSet should have size 1
      p.moviesUpserts.map(_._3.tmdbId).toSet shouldBe Set(Some(a.tmdbId), Some(b.tmdbId))
    }
  }

  it should "never mix both films' cinema data into one upserted record" in {
    forAll(genCollidingPair) { case (a, b) =>
      val p = plan(a, b)
      val (aCinemas, bCinemas) = (cinemasOf(a), cinemasOf(b))
      p.moviesUpserts.foreach { case (_, _, record) =>
        val touchesA = record.data.keySet.exists(aCinemas.contains)
        val touchesB = record.data.keySet.exists(bCinemas.contains)
        withClue(s"one upserted record carries BOTH films' cinema data: ${record.data.keySet}\n") {
          (touchesA && touchesB) shouldBe false
        }
      }
    }
  }

  it should "mint DISTINCT ids for the two colliding films" in {
    forAll(genCollidingPair) { case (a, b) =>
      val p = plan(a, b)
      withClue(s"two colliding films minted the SAME id — one write would silently clobber " +
        s"the other instead of reaching key_1: ${p.moviesUpserts.map(_._1)}\n") {
        p.moviesUpserts.map(_._1).distinct should have size p.moviesUpserts.size
      }
    }
  }

  it should "plan the identical result whichever cluster's rows arrive first" in {
    forAll(genCollidingPair, Gen.long) { case ((a, b), seed) =>
      val rows     = stagingRowsOf(a) ++ stagingRowsOf(b)
      val forward  = StagingFold.planGroup(rows, Seq.empty, titleNormalizer)
      val shuffled = StagingFold.planGroup(permute(seed, rows), Seq.empty, titleNormalizer)
      def asTriples(p: StagingFold.Plan) = p.moviesUpserts.map { case (id, k, r) => (id, k, r.tmdbId) }.toSet
      asTriples(shuffled) shouldBe asTriples(forward)
    }
  }
}
