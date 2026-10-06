package scripts

import services.identity._
import services.identity.agreement.{Agreement, VenueListings}
import services.movies.{ListingKey, TitleNormalizer}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * Every contender of every cluster the unmatched-cluster fixture's replay leaves untaken, with its signals and the facts a
 * weak signal reads (popularity, release, running time, poster distance, programme), judged by `labels.tsv` — the rows
 * a conjunction of weak signals is screened on offline before it is built.
 *
 *   sbt "worker/Test/runMain scripts.IdentityConjunctionProbe target/probe/contenders.tsv"
 */
object IdentityConjunctionProbe {
  /** The replay's allocation and time over every capture: `--alloc <runs>`. */
  private def allocation(runs: Int): Unit = {
    val captures = models.Country.all.filter(c => Files.exists(UnmatchedClusters.fixturePath(c))).map(c => UnmatchedClusters.read(UnmatchedClusters.fixturePath(c)))
    def once(): (Long, Long) = {
      val t = System.nanoTime()
      val (_, allocated) = tools.ThreadAllocation.of(captures.foreach(UnmatchedClusters.replay(_)))
      (allocated, System.nanoTime() - t)
    }
    (1 to 3).foreach(_ => once())
    val measured = (1 to runs).map(_ => once())
    System.gc(); System.gc()
    val heap = java.lang.management.ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
    val mb = measured.map(_._1 / 1e6).sorted
    val ms = measured.map(_._2 / 1e6).sorted
    println(f"replay allocation MB: median ${mb(mb.size / 2)}%.1f min ${mb.head}%.1f max ${mb.last}%.1f; ms median ${ms(ms.size / 2)}%.0f; heap after GC ${heap / 1e6}%.1f MB")
  }

  def main(args: Array[String]): Unit = {
    if (args.headOption.contains("--alloc")) { allocation(args.lift(1).fold(10)(_.toInt)); return }
    val out    = Path.of(args.headOption.getOrElse("target/probe/contenders.tsv"))
    val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val today  = java.time.LocalDate.ofInstant(tools.SpecClock.Pinned.instant(), java.time.ZoneOffset.UTC)
    val header = Seq("country", "cluster", "venues", "listings", "rawTitle", "film", "title", "right", "wrong", "taken", "contenders",
      "popularity", "year", "runtime", "released", "releasedHere", "filmCountries", "posterBits", "modelP", "rank", "listingYear",
      "listingRuntime", "listingDirector", "weighedBy", "programme", "guards", "signals")
    val rows = models.Country.all.filter(c => Files.exists(UnmatchedClusters.fixturePath(c))).flatMap { country =>
      val capture = UnmatchedClusters.read(UnmatchedClusters.fixturePath(country))
      val outcome = UnmatchedClusters.replay(capture)
      val replay  = new UnmatchedClusters.Replay(capture)
      val normalizer = TitleNormalizer.forCountry(country)
      val docs = new InMemoryTmdbDocuments
      docs.put(TmdbKind.Family, capture.families.toSeq)
      val store   = new FamilyAnswerStore(docs, tools.SpecClock.Pinned)
      val answers = UnmatchedClusters.familiesOf(country).map(f => f -> store.answers(f))
      val programmes = modules.wiring.IdentityCutoverWiring.listedOn(country.code).map(store.answers)
      val posterStore = new PosterAnswerStore(store, tools.SpecClock.Pinned)
      val byKey   = capture.listings.map(l => l.key -> l).toMap
      val code    = country.code
      outcome.model.decisions.zip(outcome.agreed.decisions).flatMap { case (model, agreed) =>
        val listings = model.members.flatMap(byKey.get)
        val takenNow = agreed.film.map(id => s"tmdb:$id").orElse(agreed.fallback.map(f => s"${f.source}:${f.id}")).getOrElse("")
        if (listings.isEmpty || agreed.basis == ResolverDecision.Basis.Event) Nil
        else {
          val nodes    = IdentityResolver.evidenceOf(listings, replay, normalizer)(_ => true)
          val verdicts = answers.flatMap { case (family, fa) =>
            Agreement.verdict(listings, fa, replay, normalizer, IdentityCalibration.resolver.withPriorSpread(family.priorSpread)).toOption }
          def page(x: Listing) = if (replay.hasDetail(x)) replay.detail(x).toOption.flatten else None
          val stated = listings.map { x =>
            page(x).fold(x) { detail =>
              val e = Evidence.of(x, Some(detail))
              x.copy(year = e.year, directors = e.directors, runtime = e.runtime, originalTitle = e.originalTitle, countries = e.countries)
            }
          }
          val distances = {
            val shown = PosterEvidence.urls(listings).flatMap(url => posterStore.venue(url).toOption.flatten)
            if (shown.isEmpty) Nil else {
              val scored = nodes.flatMap(_.candidates).filterNot(k => k.denied || FallbackIds.isFallback(k.tmdbId)).map(_.tmdbId)
              val named  = verdicts.flatMap(v => v.pick.map(_.record) ++ v.leaning).flatMap(r => r.crossIds.get("tmdb").flatMap(_.toIntOption)
                .orElse(r.crossIds.get("imdb").flatMap(capture.finds.get).flatten))
              val films  = (scored ++ named).distinct.sorted
              val hashes = films.map(f => f -> posterStore.film(f).toOption.getOrElse(Nil))
              shown.map(poster => hashes.map { case (f, held) => f -> PosterEvidence.nearest(Seq(poster), held) }.toMap)
            }
          }
          val contenders = UnifiedEvidence.contenders(UnifiedEvidence.ClusterEvidence(listings, model, nodes, verdicts, distances,
            imdb => capture.finds.get(imdb).flatten, today.getYear, stated, Some(x => Evidence.of(x, page(x)).measured)))
          val listed = programmes.flatMap(p => VenueListings.listed(listings, p).toOption.flatten)
          val candidates = nodes.flatMap(_.candidates).groupBy(_.tmdbId)
          val rawTitle = listings.map(_.rawTitle).groupBy(identity).toSeq.sortBy(t => (-t._2.size, t._1)).head._1
          contenders.map { k =>
            val verdict = listings.map(x => UnmatchedClusters.verdict(UnmatchedClusters.Take(code, x.venue, x.rawTitle, k.tmdb, k.imdb, "", k.title, k.familyIds), labels))
            val film = k.tmdb.flatMap(candidates.get).map(_.head.film).orElse(k.tmdb.flatMap(id => capture.films.get(id).flatten))
              .orElse(verdicts.flatMap(v => v.pick.map(_.record) ++ v.leaning ++ v.weighed).find(r =>
                k.imdb.exists(r.crossIds.get("imdb").contains) || k.tmdb.exists(id => r.crossIds.get("tmdb").contains(id.toString))).map(_.film))
            val open = k.tmdb.flatMap(candidates.get).getOrElse(Nil).filterNot(_.denied)
            val weighedBy = verdicts.filter(v => v.weighed.exists(r => k.imdb.exists(r.crossIds.get("imdb").contains) ||
              k.tmdb.exists(id => r.crossIds.get("tmdb").contains(id.toString)))).map(_.family.label)
            val bits = k.tmdb.toSeq.flatMap(id => distances.flatMap(_.get(id).flatten)).minOption
            Seq(code, ListingKey.serialised(model.members.minBy(ListingKey.serialised)), listings.map(_.venue).distinct.size.toString,
              listings.size.toString, rawTitle, k.film, k.title,
              verdict.count(_.contains(true)).toString, verdict.count(_.contains(false)).toString,
              if (takenNow.isEmpty) "" else if (k.film == takenNow) "this" else "other", contenders.size.toString,
              film.flatMap(_.popularity).fold("")(p => f"$p%.2f"), film.flatMap(_.year).fold("")(_.toString),
              film.flatMap(_.runtime).fold("")(_.toString), film.flatMap(_.released).fold("")(_.toString),
              film.flatMap(_.releasedIn(code.toUpperCase match { case "UK" => "GB"; case cc => cc })).fold("")(b => if (b) "1" else "0"),
              film.flatMap(_.countries).fold("")(_.mkString(",")), bits.fold("")(_.toString),
              open.map(_.probability).maxOption.fold("")(p => f"$p%.4f"), open.flatMap(_.rank).minOption.fold("")(_.toString),
              stated.flatMap(_.year).distinct.mkString(","), stated.flatMap(_.runtime).distinct.mkString(","),
              if (stated.exists(_.directors.nonEmpty)) "1" else "0", weighedBy.mkString(","),
              listed.fold("")(r => if (k.imdb.exists(r.crossIds.get("imdb").contains) || k.tmdb.exists(id => r.crossIds.get("tmdb").contains(id.toString)) ||
                k.familyIds.get("filmweb").exists(r.crossIds.get("filmweb").contains)) "this" else "other"),
              UnifiedEvidence.vetoes(n => k.signals.getOrElse(n, 0.0)).mkString(","),
              k.signals.toSeq.sortBy(_._1).map { case (n, x) => if (x == 1.0) n else f"$n=$x%.2f" }.mkString(";")
            ).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
          }
        }
      }
    }
    Files.createDirectories(out.toAbsolutePath.getParent)
    Files.writeString(out, (header.mkString("\t") +: rows).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    println(s"wrote $out: ${rows.size} rows")
  }
}
