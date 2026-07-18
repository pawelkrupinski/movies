package scripts

import models.{Country, Filmweb, Source, Tmdb}
import services.MongoConnection
import services.movies.MongoMovieRepository

/**
 * Read-only diagnostic: for a country's corpus, report WHICH source slot supplies
 * each row's effective genres and what language each `Tmdb` slot was enriched in,
 * so a "German site shows Polish genres" complaint can be pinned on the right
 * source (a stale pre-locale-fix `Tmdb` slot vs the always-Polish `Filmweb` slot
 * vs a cinema slot) instead of guessed at.
 *
 * This is also how you VERIFY the stale-language backfill: the stamp tally at the
 * bottom should converge on the deployment's own tag as `UnresolvedTmdbReaper`
 * re-resolves the wrong-language rows (it spreads them over ~24h, so expect the
 * "(unstamped → pl-PL)" bucket to drain gradually rather than all at once).
 *
 * Run against a prod tunnel:
 *   flyctl proxy 27017 -a kinowo-mongo
 *   sbt "worker/Test/runMain scripts.GenreLanguageAudit de"
 *   sbt "worker/Test/runMain scripts.GenreLanguageAudit de 'Super Mario Galaxy'"  # one film
 */
object GenreLanguageAudit {

  /** Genre names only Polish uses — enough to flag a slot as Polish-sourced. */
  private val PolishGenres = Set(
    "familijny", "komedia", "przygodowy", "animacja", "dramat", "obyczajowy",
    "sensacyjny", "kryminał", "wojenny", "historyczny", "dokumentalny",
    "muzyczny", "romans", "psychologiczny", "fantastyka", "biograficzny",
    "katastroficzny", "erotyczny", "familijne", "przygodowe")

  private def isPolish(genres: Seq[String]): Boolean =
    genres.exists(g => PolishGenres.contains(g.trim.toLowerCase))

  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.Germany)
    val filter  = args.drop(1).headOption
    val dbName  = country.mongoDb
    val conn    = MongoConnection.fromEnvForDb(dbName, required = true)
    val db = conn.database.getOrElse {
      println(s"Could not open $dbName — is the tunnel up (flyctl proxy 27017 -a kinowo-mongo)?")
      sys.exit(1)
    }
    val repo = new MongoMovieRepository(sharedDb = Some(db), fallbackToOwnInit = false)
    val all  = repo.findAll()
    val rows = all.filter(s => filter.forall(f => s.title.toLowerCase.contains(f.toLowerCase)))

    println(s"${country.displayName} ($dbName): ${all.size} rows, ${rows.size} matched\n")

    if (filter.isDefined) rows.foreach { s =>
      val r = s.record
      println(s"=== row key: '${s.title}' (${s.year.getOrElse("-")})  tmdbId=${r.tmdbId.getOrElse("-")}")
      println(s"    EFFECTIVE genres: ${r.genres.mkString(", ")}")
      println(s"    Tmdb slot language stamp: ${r.data.get(Tmdb).flatMap(_.language).getOrElse("(unstamped)")}")
      // Run the REAL display logic over the REAL stored record: if this already
      // yields a local-language title, the row is fine and any Polish still on the
      // page is a stale read-model projection, not a domain-logic bug.
      println(s"    displayTitle(from key) -> '${r.displayTitle(s.title)}'")
      println(s"    sanitize(key)='${services.movies.TitleNormalizer.sanitize(s.title)}'")
      r.data.toSeq.collectFirst { case (_, sd) if sd.title.nonEmpty =>
        println(s"    sanitize(cinema title)='${services.movies.TitleNormalizer.sanitize(sd.title.get)}'")
      }
      // The row KEY (what the page displays) is separate from any slot's title —
      // a Polish key can outlive correctly-localized slots, so print both.
      println(f"    ${"SLOT"}%-45s ${"TITLE"}%-40s GENRES")
      r.data.toSeq.sortBy(_._1.displayName).foreach { case (slot, sd) =>
        if (sd.genres.nonEmpty || sd.title.nonEmpty)
          println(f"    ${slot.displayName}%-45s ${sd.title.getOrElse("-")}%-40s ${sd.genres.mkString(", ")}")
      }
      println()
    }

    // Which slot is the effective genres list actually coming from?
    def winningSlot(r: models.MovieRecord): String =
      if (r.genres.isEmpty) "(none)"
      else if (r.data.get(Tmdb).exists(_.genres.nonEmpty)) "Tmdb"
      else if (r.data.get(Filmweb).exists(_.genres.nonEmpty)) "Filmweb"
      else "cinema"

    val bySource = rows.map(s => winningSlot(s.record))
      .groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2)
    println("effective genres supplied by:")
    bySource.foreach { case (src, n) => println(f"  $src%-12s $n") }

    def polishCount(pick: models.MovieRecord => Seq[String]) = rows.count(s => isPolish(pick(s.record)))
    def slot(so: Source)(r: models.MovieRecord) = r.data.get(so).map(_.genres).getOrElse(Seq.empty)

    println(s"\nEFFECTIVE genres look Polish: ${polishCount(_.genres)} / ${rows.size}")
    println(s"Tmdb slot holds Polish genres:    ${polishCount(slot(Tmdb))} / ${rows.size}")
    println(s"Filmweb slot holds Polish genres: ${polishCount(slot(Filmweb))} / ${rows.size}")

    // The stamp is the authoritative signal (the word-list above only catches
    // distinctly-Polish spellings, so it UNDER-counts). Anything not on the
    // deployment's own tag is what the reaper will force-re-resolve.
    val expected = country.language.toLanguageTag
    // Distinguish "no Tmdb slot at all" (never resolved — nothing for the reaper to
    // correct) from "Tmdb slot present but unstamped" (a genuinely stale legacy row
    // the reaper WILL re-resolve). Collapsing the two makes a healthy corpus look
    // like it has permanent re-resolve debt.
    val byTag = rows.map { s =>
      s.record.data.get(Tmdb) match {
        case None                              => "(no Tmdb slot — unresolved)"
        case Some(slot) if slot.language.isEmpty && slot.genres.isEmpty && slot.synopsis.isEmpty &&
                           slot.countries.isEmpty && slot.title.isEmpty => "(Tmdb slot, no localized text)"
        case Some(slot)                        => slot.language.getOrElse("(unstamped → pl-PL)")
      }
    }.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2)
    println(s"\nTmdb slot enrichment-language stamp (deployment expects $expected):")
    byTag.foreach { case (tag, n) =>
      // Only a stamped-but-wrong or unstamped-WITH-text slot is re-resolve debt.
      val due = tag == "(unstamped → pl-PL)" || (!tag.startsWith("(") && !tag.startsWith(expected))
      println(f"  $tag%-32s $n${if (due) "   ← re-resolve due" else ""}")
    }

    println("\nsample of rows with Polish effective genres:")
    rows.filter(s => isPolish(s.record.genres)).take(15).foreach { s =>
      println(s"  - ${s.title}: ${s.record.genres.mkString(", ")}  [from ${winningSlot(s.record)}]")
    }

    conn.close()
    sys.exit(0)
  }
}
