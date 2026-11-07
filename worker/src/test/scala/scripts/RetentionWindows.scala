package scripts

import services.identity.{IdentityResolver, Listing, ListingRetention, PopulationMonotonicity, ResolveInputs}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

/**
 * What a longer retention window costs, over a SYNTHETIC departure history (no store holds dated departures older than
 * 2026-11-03): every listing an older recording billed that the newest does not is a departed listing, departed at the
 * last older recording that billed it. For each window W the newest recording's corpus is resolved whole with the
 * departed listings of the last W days kept by today's rule ([[ListingRetention.retained]]: a present listing relates to
 * it by title), as the worker's model reads them, and the take of every PRESENT listing written out.
 *
 *   worker/Test/runMain scripts.RetentionWindows <cc> <newest fixtures dir> --older <corpus.json.gz>@<ISO instant> ...
 *     --windows 0,7,14,24 --out DIR
 *
 * Writes `takes-<cc>-<W>.tsv` (venue, raw title, take) and `windows-<cc>.tsv` (window, departed in it, kept, resolve
 * seconds). Model takes only. A kept listing's own searches are answered from the newest recording's tree, so a question
 * only an older billing asks is a gap.
 */
object RetentionWindows {
  def main(args: Array[String]): Unit = {
    val country = models.Country.byCode(args(0)).getOrElse(sys.error(s"no country ${args(0)}"))
    val rest    = args.drop(2).toList
    def values(flag: String): List[String] = rest.dropWhile(_ != flag).drop(1).takeWhile(!_.startsWith("--"))
    val older   = values("--older").map { spec =>
      val (path, at) = spec.splitAt(spec.lastIndexOf('@'))
      Path.of(path) -> Instant.parse(at.drop(1))
    }
    val windows = values("--windows").flatMap(_.split(',')).map(_.toInt)
    val out     = Path.of(values("--out").headOption.getOrElse(sys.error("--out DIR")))
    Files.createDirectories(out)

    val recording = IdentitySlice.Recording(Path.of(args(1)), country)
    val filed     = tools.FiledAnswers.of(country)
    val opened    = IdentitySlice.openRecording(recording, filed)
    val now       = recording.recordedAt
    val reads     = IdentitySlice.readsOf(opened, opened.fetch, opened.gaps, now, None, tools.ProdState.none)
    val present   = IdentitySlice.listingsOf(recording, opened.rows)
    val billed    = present.map(_.key).toSet
    // each departed listing as last billed, at the last older recording billing it
    val departed: Map[services.movies.ListingKey, (Listing, Instant)] = older.sortBy(_._2).foldLeft(Map.empty[services.movies.ListingKey, (Listing, Instant)]) {
      case (acc, (corpus, at)) =>
        val listings = IdentitySlice.listingsOf(recording, tools.CorpusFixture.readFrom(corpus))
        acc ++ listings.filterNot(l => billed(l.key)).map(l => l.key -> (l, at))
    }
    // warm the lookups' memo once, so the first window's time is no colder than the rest
    val _ = IdentityResolver.resolve(present, reads.lookups, reads.normalizer, ResolveInputs.Unpinned)
    val summary = new StringBuilder("window\tdeparted\tkept\tresolveSeconds\n")
    windows.foreach { days =>
      val inWindow = departed.values.filter { case (_, at) => !at.isBefore(now.minus(java.time.Duration.ofDays(days.toLong))) }.map(_._1).toSeq
      val kept     = ListingRetention.retained(inWindow, present, reads.normalizer)
      val started  = System.nanoTime()
      val resolved = IdentityResolver.resolve(present ++ kept, reads.lookups, reads.normalizer, ResolveInputs.Unpinned)
      val seconds  = (System.nanoTime() - started) / 1e9
      val lines    = present.flatMap(l => resolved.decisionOf.get(l.key).map(d =>
        s"${l.venue}\t${l.rawTitle}\t${PopulationMonotonicity.takeOf(d).getOrElse(d.film.fold("none")(id => s"tmdb:$id") + "?")}"))
      Files.write(out.resolve(s"takes-${country.code}-$days.tsv"), lines.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      summary.append(f"$days\t${inWindow.size}\t${kept.size}\t$seconds%.1f\n")
      println(f"[RetentionWindows] ${country.code} window ${days}d: departed ${inWindow.size}, kept ${kept.size}, resolve $seconds%.1fs")
    }
    Files.write(out.resolve(s"windows-${country.code}.tsv"), summary.toString.getBytes(StandardCharsets.UTF_8))
  }
}
