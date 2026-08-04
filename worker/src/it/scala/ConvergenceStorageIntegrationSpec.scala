package integration

import models.{Multikino, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.MongoStagingRepository
import tools.{ConvergenceStorage, Env}
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * That every part of a Mongo-backed convergence run looks at the SAME database.
 *
 * The repositories are handed a `MongoDatabase` directly, while `MongoStagingFolder` is
 * built from a `MongoConnection` and resolves its collections by database NAME. Those are
 * two routes to what must be one place, and nothing checked that they agreed.
 *
 * They didn't. `IsolatedMongoDatabase.nameFor` embeds `System.nanoTime()`, so generating
 * the name a second time for the connection produced a different database: staging wrote
 * 6,975 rows to one, the folder looked for them in the other, found none, and reported
 * nothing to fold. Every component behaved correctly and the corpus still never reached
 * `movies` — the suite said `resolved NOTHING — 0 films` with no error anywhere, and the
 * cause took three wrong diagnoses to find.
 *
 * Asserted through the seam that actually broke — a row written by the storage's own
 * staging repository must be visible through the storage's CONNECTION — rather than by
 * comparing two names, which would pass just as well if a third route appeared.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ConvergenceStorageIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  /** The 2026-08-04 regression, in the layer that can catch it in seconds rather
   *  than in an hour-long corpus replay.
   *
   *  A convergence storage is built once per COUNTRY leg. It used to read
   *  `TitleNormalizer.deployment`, which made the choice invisible; a mechanical
   *  sweep then filled the seam with `SingleCountryNormalizer` — Poland's — and
   *  the Germany and UK legs keyed their corpora through the Polish " & " -> " i "
   *  unification. `wallaceigromitthecurseofthewererabbit` and
   *  `patgarrettibillythekid` in a UK corpus; `bloodisinners` in a German one.
   *
   *  Asserted by BEHAVIOUR under a country whose rules differ from the default,
   *  because identity would not catch it: in a test JVM naming no country,
   *  `deployment` and `SingleCountryNormalizer` are the same memoised Poland
   *  instance. Germany is the country that disagrees, so Germany is the probe.
   *
   *  This is the ConvergenceStorage twin of `WorkerWiringNormalizerIntegrationSpec`,
   *  which has asserted the same property of the PRODUCTION root all along — the
   *  replay harness was simply never held to it. */
  it should "key through the country it was built for, not the single-country default" in {
    val de = ConvergenceStorage.mongo(
      Env.get("MONGODB_URI").get, "normalizer-scope-spec",
      services.movies.TitleNormalizer.forCountry(models.Country.Germany))
    try {
      withClue("a German leg must not fold ' & ' to the Polish ' i ': ") {
        de.movies.normalizer.sanitize("Minions & Monster")  shouldBe "minionsmonster"
        de.staging.normalizer.sanitize("Minions & Monster") shouldBe "minionsmonster"
      }
      // …and Poland's really does differ, so the assertion above is not vacuous.
      services.movies.TitleNormalizer.forCountry(models.Country.Poland)
        .sanitize("Minions & Monster") shouldBe "minionsimonster"
    } finally de.close()
  }

  "a Mongo convergence storage" should "expose one database to its repositories and its connection alike" in {
    val storage = ConvergenceStorage.mongo(Env.get("MONGODB_URI").get, "storage-agreement-spec", titleNormalizer)
    try {
      storage.staging.upsert(Multikino, "Ghost In The Shell", Some(2017), MovieRecord())

      val throughConnection = new MongoStagingRepository(storage.connection.database, normalizer = titleNormalizer).findAll()

      withClue("a row written through the storage's repository must be visible through its " +
               "connection — the folder reaches staging that way: ") {
        throughConnection.map(_.id) should contain (storage.staging.findAll().head.id)
      }
    } finally storage.close()
  }
}
