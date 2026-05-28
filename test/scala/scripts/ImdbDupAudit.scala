package scripts

import services.movies.{MongoMovieRepo, StoredMovieRecord}

/** Quick report: rows that share an imdbId — i.e. TMDB+IMDb resolved
 *  them as the same film but they live under different (title, year)
 *  cache keys. Pure read; no mutation. */
object ImdbDupAudit {
  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val rows: Seq[StoredMovieRecord] = repo.findAll()
    val byImdb = rows.filter(_.record.imdbId.isDefined).groupBy(_.record.imdbId.get)
    val dups = byImdb.filter(_._2.size > 1)

    println(s"@@ ${rows.size} rows total, ${dups.size} imdbId-collision groups")
    println()
    println("=" * 110)
    dups.toSeq.sortBy(-_._2.size).foreach { case (imdb, group) =>
      println(f"imdbId=$imdb%-12s  rows=${group.size}")
      group.foreach { r =>
        val cinemas = r.record.data.keys.map(_.displayName).toSeq.sorted.mkString(", ")
        val titleField = s"'${r.title}'"
        val yearField  = r.year.getOrElse("—").toString
        val tmdbField  = r.record.tmdbId.getOrElse("—").toString
        println(f"  $titleField%-50s year=$yearField%-6s tmdbId=$tmdbField%-10s [$cinemas]")
      }
      println("-" * 110)
    }

    repo.close()
  }
}
