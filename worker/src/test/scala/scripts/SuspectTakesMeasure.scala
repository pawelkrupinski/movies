package scripts

import services.identity.{IdentityMeasures, ResolverDecision, SuspectTakes}
import services.identity.SuspectTakes.{Signal, Take}
import tools.{CommandLine, IdentityJudgements, SuspectTakesAudit, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * How well the suspect-take signals ([[SuspectTakes]]) find the takes the identity judgements call wrong, over the takes
 * `integration.SuspectTakesDump` wrote — or a convergence leg did (`<corpus>-takes/<cc>.json.gz` among its diagnostics):
 *
 *   sbt "worker/Test/runMain scripts.SuspectTakesMeasure --dumps target/suspect-takes [--year 2026] [--out <file>] [--missed <file>]"
 *   (`--why Lalka~Michael` prints each labelled take of a billing holding one of the words, its record and suspicions)
 *
 * Every labelled (listing, film) pair the dump holds the listing and the film's record of is read as a take — today's
 * decision when today takes that film, else the film put to the listing as a take no rule made (no route to suspect) —
 * and each signal's recall on the labelled-wrong pairs and flag rate on the labelled-right ones printed; then today's
 * takes' suspects no label judges, per country: the queue a person judges. `--out` writes the queue, every reason, and
 * every take a label calls wrong (`SuspectTakesAudit.QueueHeader`); `--missed` the wrong labels no
 * signal suspects, and beside it the right ones a signal does.
 */
object SuspectTakesMeasure {

  val commandLine: CommandLine = CommandLine("SuspectTakesMeasure", "Measures the suspect-take signals against the judged takes of a takes dump.",
    CommandLine.Value("dumps", "dir", "the <cc>.json.gz takes dumps", default = Some("target/suspect-takes")),
    CommandLine.Value("year", "yyyy", "the year a take's record is read against", default = Some("2026")),
    CommandLine.Value("why", "word~word", "print each labelled take of a billing holding one of the words"),
    CommandLine.Value("out", "file", "where the queue, its reasons and the wrong takes go"),
    CommandLine.Value("missed", "file", "where the wrong labels no signal suspects go"))

  /** A labelled pair read as a take: its country and label, the signals that suspect it and why, and whether today
   *  takes that film. */
  final case class Graded(country: String, label: UnmatchedClusters.Label, signals: Set[Signal], reasons: Seq[String], today: Boolean)

  /** The film record a label's film (`tmdb:<id>`, or `imdb:tt…` by the IMDb number a record carries) names, under its
   *  TMDB key: none when the dump holds no such record. */
  def recordOf(film: String, films: Map[Int, IdentityMeasures.Film]): Option[(String, Option[Int], IdentityMeasures.Film)] = film.split(":", 2) match {
    case Array("tmdb", id) => id.toIntOption.flatMap(i => films.get(i).map(f => (s"tmdb:$i", Some(i), f)))
    case Array("imdb", id) => films.find { case (_, f) => f.imdbNumber > 0 && f"tt${f.imdbNumber}%07d" == id }.map { case (i, f) => (s"tmdb:$i", Some(i), f) }
    case _                 => None
  }

  private def pct(n: Int, d: Int) = if (d == 0) "-" else f"${100.0 * n / d}%.1f%%"

  /** Each signal's (and ANY signal's) recall on the labelled-wrong pairs and flag rate on the labelled-right ones, as a
   *  markdown table. */
  def signalTable(graded: Seq[Graded]): Seq[String] = {
    val (right, wrong) = graded.partition(_.label.right)
    "| signal | wrong flagged | recall | right flagged | flag rate on right |" +: "|---|---|---|---|---|" +:
      (Signal.values.toSeq.map(s => s.label -> ((g: Graded) => g.signals(s))) :+ ("ANY" -> ((g: Graded) => g.signals.nonEmpty))).map { case (name, on) =>
        s"| $name | ${wrong.count(on)}/${wrong.size} | ${pct(wrong.count(on), wrong.size)} | ${right.count(on)}/${right.size} | ${pct(right.count(on), right.size)} |"
      }
  }

  /** Where a wrong label came from: a take judged off a capture's or a review's page, or a pair ruled out ahead of any
   *  take — else the note's first two words. */
  def provenance(note: String): String = note.takeWhile(c => c != ':' && c != ';').split(' ').take(2).mkString(" ") match {
    case n if n.startsWith("judged") => "judged take"
    case n if n.startsWith("must not") || n.contains("must not") => "must-not pair"
    case n => n
  }

  /** `--missed`'s lines: the wrong labels no signal suspects; and its `.right` file's: the right ones a signal does. */
  def missedLines(graded: Seq[Graded]): (Seq[String], Seq[String]) = (
    graded.filter(g => !g.label.right && g.signals.isEmpty).map(g => s"${g.country}\t${g.label.venue}\t${g.label.rawTitle}\t${g.label.film}\t${g.label.note}"),
    graded.filter(g => g.label.right && g.signals.nonEmpty).map(g => s"${g.country}\t${g.label.rawTitle}\t${g.label.film}\t${g.reasons.mkString("; ")}"))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val dumps  = arguments.requiredPath("dumps")
    val year   = arguments.requiredInt("year")
    val judged = IdentityJudgements.read()
    val labels = IdentityJudgements.labels(judged)
    val tolerated = IdentityJudgements.tolerated(judged, IdentityJudgements.Scope.Served).keySet
    val countries = models.Country.all.map(c => c.code -> dumps.resolve(s"${c.code}.json.gz")).filter(p => Files.exists(p._2))

    val graded = Seq.newBuilder[Graded]
    val queue  = Seq.newBuilder[String]
    val counts = Seq.newBuilder[String]

    countries.foreach { case (cc, path) =>
      val dump  = UnmatchedClusters.read(path)
      val films  = dump.films.collect { case (id, Some(f)) => id -> f }
      val venues = new UnmatchedClusters.Replay(dump)
      val corpus = SuspectTakes.corpus(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings, venues, films.get)
      val todayOf = corpus.takes.map(t => t.listing.key -> t).toMap
      val stated  = (l: services.identity.Listing) => if (venues.hasDetail(l)) venues.detail(l).toOption.flatten.fold(l)(l.withPage) else l

      // every labelled pair as a take
      val byTitle = dump.listings.groupBy(l => UnmatchedClusters.singleSpaced(l.rawTitle))
      labels.filter(_.country == cc).foreach { label =>
        val listings = byTitle.getOrElse(UnmatchedClusters.singleSpaced(label.rawTitle), Nil).filter(l => label.venue == "*" || label.venue == l.venue)
        recordOf(label.film, films).foreach { case (film, tmdb, f) =>
          val asTakes = listings.map(stated).map { l =>
            todayOf.get(l.key).filter(t => t.tmdb == tmdb).getOrElse(
              Take(l, film, tmdb, f, ResolverDecision(Seq(l.key), tmdb, 0.0, ResolverDecision.Basis.OwnMatch, Nil)()))
          }
          if (asTakes.nonEmpty) {
            val found = asTakes.flatMap(SuspectTakes.of(_, year, corpus.billings))
            arguments.get("why").filter(_.split("~").exists(label.rawTitle.contains(_))).foreach { _ =>
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

      val verdict = SuspectTakesAudit.judge(cc, corpus, year, labels, tolerated)
      // what the worker's count of the same corpus costs, on its own thread, per written projection of the whole of it (`SuspectTakeCount`)
      val counting = tools.Stopwatch.start()
      val (_, allocated) = tools.ThreadAllocation.of(new services.identity.adapters.SuspectTakeCount(venues, ids => ids.flatMap(id => films.get(id).map(id -> _)).toMap,
        tools.SpecClock.Pinned, java.time.ZoneOffset.UTC, _ => (), _.run(), () => false, services.identity.TakeJudgements.Index.bundled(cc))(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings))
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
    println(s"labelled pairs graded: ${wrong.size} wrong (${wrong.count(_.today)} taken today), ${right.size} right (${right.count(_.today)} taken today)")
    signalTable(all).foreach(println)
    println("\nrecall on the wrong labels, by where the label came from:")
    wrong.groupBy(g => provenance(g.label.note)).toSeq.sortBy(-_._2.size).foreach { case (from, gs) =>
      println(f"  $from%-20s ${gs.count(_.signals.nonEmpty)}%3d/${gs.size}%-3d ${pct(gs.count(_.signals.nonEmpty), gs.size)}")
    }
    println("\nqueue (today's suspect takes no label judges, beyond the baseline):")
    counts.result().foreach(println)
    arguments.get("missed").foreach { file =>
      val (missed, flaggedRight) = missedLines(all)
      Files.write(Paths.get(file), missed.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      Files.write(Paths.get(file + ".right"), flaggedRight.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
    arguments.get("out").foreach { file =>
      val out: Path = Paths.get(file)
      Option(out.getParent).foreach(Files.createDirectories(_))
      Files.write(out, (SuspectTakesAudit.QueueHeader +: queue.result().distinct.sorted).mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      println(s"wrote $out")
    }
  }
}
