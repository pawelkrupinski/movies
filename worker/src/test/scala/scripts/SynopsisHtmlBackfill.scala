package scripts

import services.movies.MongoMovieRepository
import tools.TextNormalization

/**
 * One-shot: strip HTML tags from every `synopsis` field in Mongo.
 *
 * Several cinema API sources (Multikino, Cinema City, Helios, Kino
 * Pałacowe) returned HTML-wrapped synopsis text (`<div>…</div><br>`)
 * that was stored verbatim. The ingestion code now runs
 * `TextNormalization.stripHtml` on these fields; this script heals
 * the rows already persisted.
 *
 * Run: sbt "Test/runMain scripts.SynopsisHtmlBackfill"
 *
 * Delete after one successful run.
 */
object SynopsisHtmlBackfill {
  private val HtmlTagRe = "<[^>]+>".r

  def main(args: Array[String]): Unit = {
    val repository = new MongoMovieRepository()
    if (!repository.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }

    val all     = repository.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    var updated = 0

    all.foreach { r =>
      val patches = r.record.data.collect {
        case (src, sd) if sd.synopsis.exists(s => HtmlTagRe.findFirstIn(s).isDefined) =>
          src -> sd.copy(synopsis = sd.synopsis.map(TextNormalization.stripHtml).filter(_.nonEmpty))
      }
      if (patches.nonEmpty) {
        val newData = r.record.data ++ patches
        repository.upsert(r.title, r.year, r.record.copy(data = newData))
        val sources = patches.keys.mkString(", ")
        println(f"  ${r.title}%-60s [$sources]")
        updated += 1
      }
    }

    println(s"\nDone. $updated row(s) updated out of ${all.size} total.")
    if (updated > 0) {
      println("Reload the in-memory cache so the app sees the cleaned values:")
      println("  curl -X POST http://localhost:9000/debug/rehydrate")
    }

    repository.close()
  }
}
