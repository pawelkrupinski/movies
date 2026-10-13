package scripts

import services.identity.{IdentityMeasures, ResolverDecision, SuspectTakes}
import services.identity.SuspectTakes.{Signal, Take}
import tools.{SuspectTakesAudit, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * How well the suspect-take signals ([[SuspectTakes]]) find the takes `labels.tsv` calls wrong, over the takes
 * `integration.SuspectTakesDump` wrote — or a convergence leg did (`<corpus>-takes/<cc>.json.gz` among its diagnostics):
 *
 *   sbt "worker/Test/runMain scripts.SuspectTakesMeasure --dumps target/suspect-takes [--year 2026] [--out <file>] [--missed <file>]"
 *   (`--why Lalka~Michael` prints each labelled take of a billing holding one of the words, its record and suspicions)
 *
 * Every labelled (listing, film) pair the dump holds the listing and the film's record of is read as a take — today's
 * decision when today takes that film, else the film put to the listing as a take no rule made (no route to suspect) —
 * and each signal's recall on the labelled-wrong pairs and flag rate on the labelled-right ones printed; then today's
 * takes' suspects no label judges, per country: the queue a person judges. `--out` writes the queue, every reason, and
 * every take a label calls wrong, in the baseline's form (`SuspectTakesAudit.BaselinePath`); `--missed` the wrong labels no
 * signal suspects, and beside it the right ones a signal does.
 */
object SuspectTakesMeasure {

  def main(args: Array[String]): Unit = {
    val opts   = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val dumps  = Paths.get(opts.getOrElse("dumps", "target/suspect-takes"))
    val year   = opts.get("year").map(_.toInt).getOrElse(2026)
    val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val baseline = SuspectTakesAudit.readBaseline()
    val countries = models.Country.all.map(c => c.code -> dumps.resolve(s"${c.code}.json.gz")).filter(p => Files.exists(p._2))

    final case class Graded(country: String, label: UnmatchedClusters.Label, signals: Set[Signal], reasons: Seq[String], today: Boolean)
    val graded = Seq.newBuilder[Graded]
    val queue  = Seq.newBuilder[String]
    val counts = Seq.newBuilder[String]

    countries.foreach { case (cc, path) =>
      val dump  = UnmatchedClusters.read(path)
      val films  = dump.films.collect { case (id, Some(f)) => id -> f }
      val byImdb = films.toSeq.filter(_._2.imdbNumber > 0).map { case (id, f) => f"tt${f.imdbNumber}%07d" -> (id, f) }.toMap
      val venues = new UnmatchedClusters.Replay(dump)
      val corpus = SuspectTakes.corpus(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings, venues, films.get)
      val todayOf = corpus.takes.map(t => t.listing.key -> t).toMap
      val stated  = (l: services.identity.Listing) => if (venues.hasDetail(l)) venues.detail(l).toOption.flatten.fold(l)(l.withPage) else l

      // every labelled pair as a take
      labels.filter(_.country == cc).foreach { label =>
        val listings = dump.listings.filter(l => l.rawTitle == label.rawTitle && (label.venue == "*" || label.venue == l.venue))
        val record: Option[(String, Option[Int], IdentityMeasures.Film)] = label.film.split(":", 2) match {
          case Array("tmdb", id) => id.toIntOption.flatMap(i => films.get(i).map(f => (s"tmdb:$i", Some(i), f)))
          case Array("imdb", id) => byImdb.get(id).map { case (i, f) => (s"tmdb:$i", Some(i), f) }
          case _ => None
        }
        record.foreach { case (film, tmdb, f) =>
          val asTakes = listings.map(stated).map { l =>
            todayOf.get(l.key).filter(t => t.tmdb == tmdb).getOrElse(
              Take(l, film, tmdb, f, ResolverDecision(Seq(l.key), tmdb, 0.0, ResolverDecision.Basis.OwnMatch, Nil)()))
          }
          if (asTakes.nonEmpty) {
            val found = asTakes.flatMap(SuspectTakes.of(_, year, corpus.billings))
            opts.get("why").filter(_.split("~").exists(label.rawTitle.contains(_))).foreach { _ =>
              asTakes.foreach(t => println(s"WHY ${t.listing.venue} | ${t.listing.rawTitle} | clean '${t.listing.cleanTitle}' year ${t.listing.year} " +
                s"runtime ${t.listing.runtime} venues ${corpus.billings.venuesBilling(t.listing)} → ${t.film} ${t.record.title} " +
                s"${(t.record.title +: (t.record.originalTitle.toSeq ++ t.record.alternativeTitles)).mkString("[", "; ", "]")} ${t.record.year} ${t.record.runtimes} ${t.decision.basis}: " +
                SuspectTakes.of(t, year, corpus.billings).map(_.render).mkString("; ")))
            }
            graded += Graded(cc, label, found.map(_.signal).toSet, found.map(_.render).distinct,
              asTakes.exists(t => todayOf.get(t.listing.key).exists(_.film == t.film)))
          }
        }
      }

      val verdict = SuspectTakesAudit.judge(cc, corpus, year, labels, baseline)
      // what the worker's count of the same corpus costs, on its own thread, per written projection of the whole of it (`SuspectTakeCount`)
      val counting = tools.Stopwatch.start()
      val (_, allocated) = tools.ThreadAllocation.of(new services.identity.SuspectTakeCount(venues, ids => ids.flatMap(id => films.get(id).map(id -> _)).toMap,
        tools.SpecClock.Pinned, java.time.ZoneOffset.UTC, _ => (), _.run(), () => false)(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings))
      val (again, reading) = tools.ThreadAllocation.of(SuspectTakes.corpus(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings, venues, films.get))
      val (_, billing) = tools.ThreadAllocation.of(SuspectTakes.Billings.of(dump.listings, again.takes))
      val (_, asking) = tools.ThreadAllocation.of(SuspectTakes.suspected(again.takes, year, again.billings))
      val cost = f"; the worker's count: ${counting.seconds}%.1fs, ${allocated / 1000000} MB allocated (corpus ${reading / 1000000}, of it billings ${billing / 1000000}; suspicions ${asking / 1000000})"
      queue ++= verdict.fresh.map(_.line) ++ verdict.wrong.map(SuspectTakesAudit.wrongLine(cc, _))
      counts += f"$cc%-3s takes ${corpus.takes.size}%6d listings; suspects ${verdict.suspects.size}%5d billings; unjudged ${verdict.fresh.size}%5d; " +
        s"labelled wrong still taken ${verdict.wrong.map(t => (t.listing.rawTitle, t.film)).distinct.size}; by signal " +
        Signal.values.map(s => s"${s.label} ${verdict.fresh.count(_.signals.contains(s))}").mkString(", ") + cost
    }

    val all = graded.result()
    val wrong = all.filter(!_.label.right)
    val right = all.filter(_.label.right)
    def pct(n: Int, d: Int) = if (d == 0) "-" else f"${100.0 * n / d}%.1f%%"
    println(s"labelled pairs graded: ${wrong.size} wrong (${wrong.count(_.today)} taken today), ${right.size} right (${right.count(_.today)} taken today)")
    println("| signal | wrong flagged | recall | right flagged | flag rate on right |\n|---|---|---|---|---|")
    (Signal.values.toSeq.map(s => s.label -> ((g: Graded) => g.signals(s))) :+ ("ANY" -> ((g: Graded) => g.signals.nonEmpty))).foreach { case (name, on) =>
      println(s"| $name | ${wrong.count(on)}/${wrong.size} | ${pct(wrong.count(on), wrong.size)} | ${right.count(on)}/${right.size} | ${pct(right.count(on), right.size)} |")
    }
    // where a wrong label came from: a take judged off a capture's or a review's page, or a pair ruled out ahead of any take
    def provenance(g: Graded): String = g.label.note.takeWhile(c => c != ':' && c != ';').split(' ').take(2).mkString(" ") match {
      case n if n.startsWith("judged") => "judged take"
      case n if n.startsWith("must not") || n.contains("must not") => "must-not pair"
      case n => n
    }
    println("\nrecall on the wrong labels, by where the label came from:")
    wrong.groupBy(provenance).toSeq.sortBy(-_._2.size).foreach { case (from, gs) =>
      println(f"  $from%-20s ${gs.count(_.signals.nonEmpty)}%3d/${gs.size}%-3d ${pct(gs.count(_.signals.nonEmpty), gs.size)}")
    }
    println("\nqueue (today's suspect takes no label judges, beyond the baseline):")
    counts.result().foreach(println)
    opts.get("missed").foreach { file =>
      Files.write(Paths.get(file), wrong.filter(_.signals.isEmpty).map(g => s"${g.country}\t${g.label.venue}\t${g.label.rawTitle}\t${g.label.film}\t${g.label.note}")
        .mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      Files.write(Paths.get(file + ".right"), right.filter(_.signals.nonEmpty).map(g => s"${g.country}\t${g.label.rawTitle}\t${g.label.film}\t${g.reasons.mkString("; ")}")
        .mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
    opts.get("out").foreach { file =>
      val out: Path = Paths.get(file)
      Option(out.getParent).foreach(Files.createDirectories(_))
      Files.write(out, (SuspectTakesAudit.BaselineHeader +: queue.result().distinct.sorted).mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      println(s"wrote $out")
    }
  }
}
