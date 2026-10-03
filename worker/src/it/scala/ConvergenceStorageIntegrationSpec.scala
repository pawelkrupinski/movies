package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.ConvergenceStorage
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Properties of a Mongo-backed convergence storage: it keys through the country it was built
 * for, and its connection is its own client, closed with it.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ConvergenceStorageIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  /** The 2026-08-04 regression, in the layer that can catch it in seconds rather
   *  than in an hour-long corpus replay.
   *
   *  A convergence storage is built once per COUNTRY leg. It used to read an
   *  environment-resolved default normalizer, which made the choice invisible; a mechanical
   *  sweep then filled the seam with `SingleCountryNormalizer` — Poland's — and
   *  the Germany and UK legs keyed their corpora through the Polish " & " -> " i "
   *  unification. `wallaceigromitthecurseofthewererabbit` and
   *  `patgarrettibillythekid` in a UK corpus; `bloodisinners` in a German one.
   *
   *  Asserted by BEHAVIOUR under a country whose rules differ from the default,
   *  because identity would not catch it: every normalizer is a fresh instance, and
   *  two Poland instances key identically. Germany is the country that disagrees,
   *  so Germany is the probe.
   *
   *  This is the ConvergenceStorage twin of `WorkerWiringNormalizerIntegrationSpec`,
   *  which has asserted the same property of the PRODUCTION root all along — the
   *  replay harness was simply never held to it. */
  it should "key through the country it was built for, not the single-country default" in {
    val de = ConvergenceStorage.mongo(
      mongoTarget, "normalizer-scope-spec",
      services.movies.TitleNormalizer.forCountry(models.Country.Germany))
    try {
      withClue("a German leg must not fold ' & ' to the Polish ' i ': ") {
        de.movies.normalizer.sanitize("Minions & Monster") shouldBe "minionsmonster"
      }
      // …and Poland's really does differ, so the assertion above is not vacuous.
      services.movies.TitleNormalizer.forCountry(models.Country.Poland)
        .sanitize("Minions & Monster") shouldBe "minionsimonster"
    } finally de.close()
  }

  /** The storage's connection is the storage's own client, not a second one built from its URI. A
   *  second one was built by `MongoConnection`'s rules, which compress every message with zlib on a
   *  loopback URI (they take loopback for the ssh tunnel to prod) — and a convergence leg's MongoDB is
   *  a loopback one: on the US leg that was ~6 GB of zlib a run on both sides of the socket, every
   *  identity collection and the task queue read and written through it, an identical re-scrape tick
   *  ~9 s instead of ~4.5 s. It was also never closed. Closed with the storage, it is the same client. */
  "a Mongo convergence storage" should "reach its database through its own client, closed with it" in {
    val storage = ConvergenceStorage.mongo(mongoTarget, "storage-client-spec", titleNormalizer)
    val database = storage.connection.database.get
    storage.close()
    withClue("the storage's connection still read after the storage closed — it holds a client of its own: ") {
      a[IllegalStateException] should be thrownBy
        { import org.mongodb.scala.ObservableFuture; scala.concurrent.Await.result(database.listCollectionNames().toFuture(), scala.concurrent.duration.DurationInt(10).seconds) }
    }
  }
}
