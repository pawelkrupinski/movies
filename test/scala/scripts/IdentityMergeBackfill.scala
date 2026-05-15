package scripts

import services.enrichment.{MovieCache, MovieRepo, IdentityMerger}

/**
 * One-shot: walk every cached row and invoke `IdentityMerger.mergeForTrigger`
 * so existing duplicates that share a tmdbId or imdbId get collapsed. After
 * this lands, the event-driven merge keeps things tidy on every subsequent
 * `TmdbResolved` / `ImdbIdMissing` — but historical state needs this single
 * sweep to catch up.
 *
 * Idempotent: re-running the script after a clean Mongo finds no siblings
 * and reports zero merges.
 *
 * Run: sbt "Test/runMain scripts.IdentityMergeBackfill"
 */
object IdentityMergeBackfill {
  def main(args: Array[String]): Unit = {
    val repo  = new MovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }
    val cache  = new MovieCache(repo)
    val merger = new IdentityMerger(cache)

    val before = cache.snapshot().size
    println(s"@@ $before rows hydrated from Mongo")

    val startedAt = System.currentTimeMillis()
    // Snapshot keys upfront because mergeForTrigger removes losers from
    // `entries` mid-iteration; re-fetching the live snapshot per loop would
    // cause us to skip indices.
    // Snapshot keys upfront because mergeFor removes losers from the cache
    // mid-iteration; re-fetching live would cause us to skip indices.
    repo.findAll().foreach { case (title, year, _) =>
      merger.mergeFor(title, year)
    }

    val after = cache.snapshot().size
    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    println()
    println("════ Summary ════")
    println(s"  rows before:  $before")
    println(s"  rows after:   $after")
    println(s"  collapsed:    ${before - after}")
    println(f"  done in $elapsedSec%.1fs")

    repo.close()
  }
}
