package scripts

import models.Country
import org.mongodb.scala.MongoClient
import services.scrapes.MongoScrapeArchiveRepository
import tools.{CorpusFixture, CountryScrapeCorpus, Env, TunnelTunedUri}

/**
 * Dump one country's real `cinema_scrapes` to a compressed fixture file.
 *
 * Split out of the convergence spec deliberately. The spec captured the corpus as
 * a SIDE EFFECT of its fallback path, which meant recording a corpus cost a full
 * convergence run — scrape, fold, settle, project, enrich, three order-independent
 * passes — when the work is a single collection read. This does only that, so the
 * nightly recording is minutes rather than an hour.
 *
 * Refuses to write anything it isn't sure of. `findAll` discards an incomplete
 * keyset scan (it returns empty rather than a short result), and this then refuses
 * an empty read — because a fixture is AUTHORITATIVE: a truncated capture is
 * replayed as the corpus on every future run, and one already slipped through at
 * 236 of 281 Polish venues before that guard existed.
 *
 * Run with:
 *   KINOWO_COUNTRY=pl KINOWO_CONVERGENCE_SCRAPES_URI=... \
 *     sbt "worker/Test/runMain scripts.RecordCorpusFixture"
 */
object RecordCorpusFixture {

  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(code => Country.all.find(_.code == code)).getOrElse(Country.fromEnv)
    val uri = Env.get("KINOWO_CONVERGENCE_SCRAPES_URI").orElse(Env.get("MONGODB_URI")).getOrElse {
      System.err.println("[corpus] set KINOWO_CONVERGENCE_SCRAPES_URI (or MONGODB_URI) to the archive source")
      sys.exit(1)
    }
    val databaseName = Env.get("KINOWO_CONVERGENCE_SCRAPES_DB").getOrElse(country.mongoDb)

    // Tunnel-tuned: this runs across a `flyctl proxy` in CI, where the default 30s
    // server selection turns a two-second proxy restart into minutes of blocking.
    val client   = MongoClient(TunnelTunedUri(uri))
    val archive  = new MongoScrapeArchiveRepository(Some(client.getDatabase(databaseName)))
    val known    = CountryScrapeCorpus.cinemasOf(country).toSet

    try {
      val rows = archive.findAll().filter(row => known.contains(row.cinema) && row.films.nonEmpty)
      if (rows.isEmpty) {
        System.err.println(
          s"[corpus] ${country.displayName}: read came back empty across all ${known.size} catalogue cinemas. " +
          "The archive's reads are best-effort and discard an incomplete scan, so this is a failed or dropped " +
          "read far more likely than an empty archive — refusing to write a fixture from it.")
        sys.exit(1)
      }

      val path = CorpusFixture.write(country.code, rows)
      val raw  = CorpusFixture.renderedBytes(rows)
      val gz   = java.nio.file.Files.size(path)
      println(s"[corpus] ${country.displayName}: ${rows.size} venues, ${rows.map(_.films.size).sum} listings")
      println(f"[corpus] wrote $path%s — ${raw / 1048576.0}%.1f MB JSON, ${gz / 1048576.0}%.2f MB gzipped")
    } finally client.close()
  }
}
