package scripts

import models.Country
import org.mongodb.scala.MongoClient
import services.scrapes.MongoScrapeArchiveRepository
import tools.{Alongside, CorpusFixture, CorpusSample, CountryScrapeCorpus, ProdCoverage, ProdCoverageBaseline, ShadowCoverage, TunnelTunedUri}

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
 *     sbt "worker/Fixtures/runMain scripts.RecordCorpusFixture"
 */
object RecordCorpusFixture {

  def main(args: Array[String]): Unit = {
    val configuration = _root_.settings.ProcessConfiguration.resolve()
    val country = args.headOption.flatMap(code => Country.all.find(_.code == code)).getOrElse(configuration.country)
    val uri = configuration.convergenceScrapesUri.map(_.value).orElse(configuration.mongoAddress.uri.map(_.value)).getOrElse {
      System.err.println("[corpus] set KINOWO_CONVERGENCE_SCRAPES_URI (or MONGODB_URI) to the archive source")
      sys.exit(1)
    }
    val databaseName = configuration.convergenceScrapesDatabase.map(_.value).getOrElse(country.mongoDb)

    // Tunnel-tuned: this runs across a `flyctl proxy` in CI, where the default 30s
    // server selection turns a two-second proxy restart into minutes of blocking.
    val client   = MongoClient(TunnelTunedUri(uri))
    val archive  = new MongoScrapeArchiveRepository(Some(client.getDatabase(databaseName)))
    val known    = CountryScrapeCorpus.cinemasOf(country).toSet

    try {
      // Prod's coverage of the repertoire, read BESIDE the archive rather than after it: its
      // aggregations are server-side counts on other collections, ~7 s of the US recording's
      // critical path behind the corpus read (run 37111868620), and captured together they are
      // only closer to the same instant — the reason they are captured here at all (below).
      val database = client.getDatabase(databaseName)
      val read     = tools.Stopwatch.start()
      val (rows, baseline) = Alongside(
        archive.findAll().filter(row => known.contains(row.cinema) && row.films.nonEmpty))(
        // …and what production's NEW model decided for it (its latest shadow run), which a cut-over
        // leg must reproduce. Only the full corpus: the sample's films are resolved apart from the rest.
        ProdCoverage.of(database).copy(shadow = ShadowCoverage.latest(database)))
      println(f"[corpus] read ${rows.size} venues and prod's coverage in ${read.seconds}%.1fs")
      if (rows.isEmpty) {
        System.err.println(
          s"[corpus] ${country.displayName}: read came back empty across all ${known.size} catalogue cinemas. " +
          "The archive's reads are best-effort and discard an incomplete scan, so this is a failed or dropped " +
          "read far more likely than an empty archive — refusing to write a fixture from it.")
        sys.exit(1)
      }

      val written = tools.Stopwatch.start()
      val CorpusFixture.Written(path, raw) = CorpusFixture.write(country.code, rows)
      val gz   = java.nio.file.Files.size(path)
      println(s"[corpus] ${country.displayName}: ${rows.size} venues, ${rows.map(_.films.size).sum} listings")
      println(f"[corpus] wrote $path%s — ${raw / 1048576.0}%.1f MB JSON, ${gz / 1048576.0}%.2f MB gzipped, in ${written.seconds}%.1fs")

      // What prod has ENRICHED for this same repertoire, read above from the connection the
      // corpus came through. This is the only moment the two can be captured together, and
      // together is the only way they compare: the corpus is what was screening at instant T,
      // and the baseline is prod's coverage of exactly that set at exactly T. Recorded anywhere
      // else it would drift against the corpus and the band it guards would become a flake.
      val baselinePath = ProdCoverageBaseline.write(country.code, baseline)
      println(s"[corpus] wrote $baselinePath — prod's coverage of the same repertoire" +
        baseline.shadow.fold(" (no shadow run)")(s => s", and its new model's ${s.films} films, ${s.tmdbId} on TMDB (shadow run ${s.runAt})"))

      // …and the same pair again over a ~100-film slice, for the fast leg that runs
      // ahead of the full matrix. The draw happens HERE, once, and the files pin it:
      // the leg replaying them is exactly reproducible, while the slice still rotates
      // every time the corpus is re-recorded, so it never ossifies around one hundred
      // films that happen to work. The seed is printed so a capture can be re-derived
      // from a log if anyone ever needs to.
      val seed   = java.time.Instant.now().toEpochMilli
      // The corpus is this country's, so the sample must be drawn under its rules.
      val titles = services.movies.TitleNormalizer.forCountry(country)
      val sample = CorpusSample.draw(rows, CorpusSample.DefaultSize, new scala.util.Random(seed), titles)
      val keys   = CorpusSample.filmKeys(sample, titles).toSet
      val sampleKey  = s"${country.code}-sample"
      val samplePath = CorpusFixture.write(sampleKey, sample).path
      // Slot keys of the SAMPLE, not of the whole corpus: a wide release is replayed from
      // only the venues the draw kept, so the baseline counts prod's rows for those.
      val sampleBaseline = ProdCoverageBaseline.write(sampleKey, ProdCoverage.of(database, onlySlotKeys = Some(CorpusSample.slotKeysOf(sample, keys, titles))))
      println(s"[corpus] sample seed $seed — ${keys.size} films drawn from ${CorpusSample.filmKeys(rows, titles).size}")
      println(s"[corpus] wrote $samplePath — ${sample.size} venues, ${sample.map(_.films.size).sum} listings, " +
              s"${sample.map(_.films.map(_.showtimes.size).sum).sum} showtimes")
      println(s"[corpus] wrote $sampleBaseline — prod's coverage of just those films")
    } finally client.close()
  }
}
