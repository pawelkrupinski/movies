package modules

import settings.ProcessConfiguration

/**
 * The worker image's AOT-cache training run: boots the real worker ([[ReplayWorkerWiring]] — every
 * fetch replayed from a recorded corpus, a real Mongo) for a fixed time, then exits, and the JVM
 * started with `-XX:AOTCacheOutput` writes into the cache the classes it loaded — the class list from
 * production heap dumps plus what only running the worker creates (~15.9k against ~13.7k) — and the
 * method profiles it gathered. Archived, neither is rebuilt in metaspace at every restart: 6–10 MB
 * less metaspace per worker (2026-10-02), most of it the profile data.
 *
 * What the profiles did NOT do is save CPU. They cut JIT 31% on the replayed boot locally, but a
 * production boot is mostly the identity take-up and cache hydration over the full corpus, not this
 * Polish scrape replay: JIT and CPU held level on PL/UK/US/DE, and worker-es — whose scrapers a Polish
 * profile steers wrong — lost ~13% of its boot CPU, so it runs with `-XX:-AOTReplayTraining`.
 *
 * CI runs it from the built image itself, so the classpath it trains on is the one production
 * launches — the JVM refuses a cache trained on any other.
 *
 * It first loads every class production loads (`tools.ClassArchiveTraining.loadFrom`, the class
 * list from production heap dumps), so the cache keeps them out of metaspace as the class-only cache
 * did — `worker-pl` died of `OutOfMemoryError: Metaspace` at its 128m cap before there was one —
 * and only then replays, which adds what running creates.
 *
 * {{{
 *   bin/worker -main modules.AotTrainingMain <lib directory> <fixture directory> <seconds>
 *   bin/worker -main modules.AotTrainingMain check     # exits at once: with -XX:AOTMode=on, proof the cache maps
 * }}}
 *
 * The Mongo is the exported `MONGODB_URI` / `MONGODB_DB`; the corpus root `KINOWO_FIXTURE_ROOT`.
 */
object AotTrainingMain {

  def main(args: Array[String]): Unit = args.toList match {
    case "check" :: Nil =>
      println("[aot-training] check: started")
    case lib :: fixtureDirectory :: seconds :: Nil =>
      tools.ClassArchiveTraining.loadFrom(java.nio.file.Paths.get(lib))
      val process = ProcessConfiguration.resolveExported()
      val wiring  = new ReplayWorkerWiring(fixtureDirectory, process.mongoAddress, process.fixtureRoot, process.env)
      println(s"[aot-training] replaying $fixtureDirectory for ${seconds}s")
      wiring.start()
      Thread.sleep(seconds.toLong * 1000L)
      try wiring.stop()
      finally sys.exit(0)
    case _ =>
      System.err.println("usage: AotTrainingMain <lib directory> <fixture directory> <seconds> | check")
      sys.exit(2)
  }
}
