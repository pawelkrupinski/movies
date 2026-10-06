package integration

import play.api.libs.json.Json
import services.identity.{DecorationSegments, DecorationTokens, IdentityMeasures, TitleDecorations}
import services.identity.ListingShape
import services.movies.{ListingKey, TitleContainment}
import tools.ConvergenceStorage

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Try

/**
 * Decoration stripping learned SUPERVISED ([[DecorationTokens]]) from the recorded full corpora: every listing the
 * model matches, its title aligned with its film's titles, labels each word decoration or the film's; a per-word
 * logistic regression is fitted on 80% of the venues and scored on the other 20% (a search title exactly the aligned
 * one); then the edge runs it strips off the UNMATCHED listings are written as proposals for the detector to measure
 * (`IdentityDecorationCandidates --proposals <out>/proposals.tsv`).
 *
 *   sbt "worker/IntegrationTest/runMain integration.IdentityDecorationTokens --out <dir>"   (the detector's environment)
 */
object IdentityDecorationTokens {

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val out  = Paths.get(opts.getOrElse("out", sys.error("--out <dir>")))
    val storages = mutable.ListBuffer.empty[ConvergenceStorage]
    Files.createDirectories(out)
    try {
      val corpora = new DecorationCorpora(settings.ProcessConfiguration.resolve(), storages)
      import corpora.{byKey, records}
      val base  = TitleDecorations.resolver
      val taken = corpora.resolveAll(base)

      // how widely the corpus uses each word
      val venuesOf = mutable.HashMap.empty[String, mutable.Set[String]]
      val filmsOf  = mutable.HashMap.empty[String, mutable.Set[(String, Int)]]
      byKey.foreach { case (k @ (cc, _), x) =>
        (TitleContainment.tokens(x.title) ++ TitleContainment.tokens(x.rawTitle)).distinct.foreach { t =>
          venuesOf.getOrElseUpdate(t, mutable.Set.empty) += x.venue
          filmsOf.getOrElseUpdate(t, mutable.Set.empty) += (cc -> taken(k).cluster)
        }
      }
      val inRecords = mutable.HashMap.empty[String, Int]
      records.distinct.foreach(r => TitleContainment.tokens(r).distinct.foreach(t => inRecords(t) = inRecords.getOrElse(t, 0) + 1))
      val spreads = mutable.HashMap.empty[String, DecorationTokens.Spread]
      val spread: String => DecorationTokens.Spread = t => spreads.getOrElseUpdate(t,
        DecorationTokens.Spread(venuesOf.get(t).fold(0)(_.size), filmsOf.get(t).fold(0)(_.size), inRecords.getOrElse(t, 0)))

      def double(x: services.identity.Listing) =
        ListingShape.billsSeveral(x) || IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing(x.title, Some(x.rawTitle), decorations = base))
      def heldOut(venue: String) = Math.floorMod(scala.util.hashing.MurmurHash3.stringHash(venue), 5) == 0

      // the training titles: each matched listing's (venue, raw title) once, aligned with its film's titles
      final case class Example(cc: String, venue: String, raw: String, words: IndexedSeq[DecorationTokens.Word], aligned: DecorationTokens.Aligned)
      val counts = mutable.Map.empty[(String, String), Int].withDefaultValue(0)
      val examples = byKey.toSeq.filter { case (k, _) => taken(k).film.startsWith("tmdb:") }
        .map { case (k @ (cc, _), x) => (cc, x.venue, x.rawTitle) -> (k, x) }.toMap.toSeq.sortBy(_._1).flatMap { case ((cc, venue, raw), (k, x)) =>
          val example = if (double(x)) { counts((cc, "double")) += 1; None }
          else DecorationTokens.words(raw).zip(DecorationTokens.align(raw, taken(k).titles)).map { case (ws, a) => Example(cc, venue, raw, ws, a) }
          if (example.isEmpty && !double(x)) counts((cc, "unaligned or ambiguous")) += 1
          example.foreach(e => counts((cc, if (e.aligned.clean) "clean" else "decorated")) += 1)
          example
        }
      val (test, train) = examples.partition(e => heldOut(e.venue))
      val rows  = train.flatMap(e => e.words.indices.map(i => DecorationTokens.features(e.words, i, spread) -> e.aligned.decoration(i)))
      val model = DecorationTokens.fit(rows)
      println(s"fitted on ${train.size} titles (${rows.size} words); weights: " + DecorationTokens.Names.zip(model.weights).map { case (n, w) => s"$n=$w" }.mkString(", "))

      // held-out venues: is the search title exactly the aligned one — the model's, stripping nothing, the learned decorations'
      def exact(e: Example, inner: Seq[String]) = inner == e.aligned.inner
      val learnedExact = (e: Example) => (Seq(e.raw) ++ base.strip(e.raw)).map(TitleContainment.tokens).exists(_ == e.aligned.inner)
      val byCountry = test.groupBy(_.cc).toSeq.sortBy(_._1).map { case (cc, es) =>
        val dec = es.filterNot(_.aligned.clean)
        def rate(xs: Seq[Example], f: Example => Boolean) = if (xs.isEmpty) "-" else f"${xs.count(f)}/${xs.size}"
        s"$cc: model ${rate(es, e => exact(e, model.inner(e.words, spread)))} (decorated ${rate(dec, e => exact(e, model.inner(e.words, spread)))}), " +
          s"no strip ${rate(es, e => exact(e, e.words.map(_.token)))}, learned decorations ${rate(es, learnedExact)} (decorated ${rate(dec, learnedExact)})"
      }

      // the unmatched listings: the edge runs a model strips, as proposals for the detector
      val known = (r: (String, Seq[String])) => (if (r._1 == "prefix") base.prefixes else base.suffixes)(r._2)
      val unmatched = byKey.toSeq.filter { case (k, x) => taken(k).film.isEmpty && !double(x) }.sortBy(_._2.sortKey)
      def proposals(innerOf: ((String, ListingKey), services.identity.Listing) => Option[Seq[String]]) = unmatched.flatMap { case (k, x) =>
        innerOf(k, x).toSeq.flatMap { inner =>
          val tokens = TitleContainment.tokens(x.rawTitle)
          val start  = tokens.indexOfSlice(inner)
          if (start < 0) Nil
          else Seq("prefix" -> tokens.take(start), "suffix" -> tokens.drop(start + inner.size)).filter(_._2.nonEmpty).filterNot(known).map(run => run -> (x.venue, inner))
        }
      }.groupMap(_._1)(_._2).toSeq.map { case ((side, run), seen) => (side, run.mkString(" "), seen.map(_._2).distinct.size, seen.map(_._1).distinct.size, seen.size) }
        .sortBy(r => (-r._3, r._1, r._2))
      val tokenProposals = proposals((_, x) => DecorationTokens.words(x.rawTitle).map(ws => model.inner(ws, spread)))

      // ── the SEGMENT model: delimiters, every signal group, and an ablation per group ──
      val segKeys = byKey.toSeq.map { case (k @ (cc, _), x) => (k, x, DecorationSegments.segments(x.rawTitle)) }
      val segVenues = mutable.HashMap.empty[String, mutable.Set[String]]
      val segFilms  = mutable.HashMap.empty[String, mutable.Set[(String, Int)]]
      val segChains = mutable.HashMap.empty[String, mutable.Set[String]]
      segKeys.foreach { case (k @ (cc, _), x, segs) => segs.toSeq.flatten.map(_.key).distinct.foreach { key =>
        segVenues.getOrElseUpdate(key, mutable.Set.empty) += x.venue
        segFilms.getOrElseUpdate(key, mutable.Set.empty) += (cc -> taken(k).cluster)
        segChains.getOrElseUpdate(key, mutable.Set.empty) += x.venue.takeWhile(_ != ' ')
      } }
      val recordKeys  = records.iterator.map(r => TitleContainment.tokens(r).mkString(" ")).toSet
      val recordWords = records.iterator.flatMap(TitleContainment.tokens).toSet
      val plainKeys   = byKey.values.flatMap(x => Seq(x.title, x.rawTitle)).map(t => TitleContainment.tokens(t).mkString(" ")).toSet
      val knownKeys   = (base.prefixes ++ base.suffixes).map(_.mkString(" "))
      val clusterTitles = byKey.toSeq.groupMap { case (k @ (cc, _), _) => (cc, taken(k).cluster) } { case (k, x) => k -> TitleContainment.tokens(x.rawTitle).mkString(" ") }
      // the venue prior: per venue and place, how often the matched titles' segment there was decoration (leave-one-out)
      final case class SegExample(e: Example, segs: IndexedSeq[DecorationSegments.Segment], labels: IndexedSeq[Boolean], key: (String, ListingKey))
      val keyOf = byKey.toSeq.map { case (k @ (cc, _), x) => (cc, x.venue, x.rawTitle) -> k }.toMap
      val segExamples = examples.flatMap(e => DecorationSegments.segments(e.raw).filterNot(DecorationSegments.billsSeveral)
        .flatMap(segs => DecorationSegments.labels(segs, e.aligned).map(l => SegExample(e, segs, l, keyOf((e.cc, e.venue, e.raw))))))
      val (segTest, segTrain) = segExamples.partition(x => heldOut(x.e.venue))
      val priorCounts = mutable.HashMap.empty[(String, String), (Int, Int)].withDefaultValue((0, 0))
      segTrain.foreach(x => x.segs.indices.foreach { i =>
        val p = (x.e.venue, DecorationSegments.place(x.segs, i)); val (d, n) = priorCounts(p)
        priorCounts(p) = (d + (if (x.labels(i)) 1 else 0), n + 1)
      })
      def contextOf(k: (String, ListingKey), x: services.identity.Listing, own: Option[(String, Boolean)] = None): DecorationSegments.Context =
        new DecorationSegments.Context {
          private val siblings = clusterTitles.getOrElse((k._1, taken(k).cluster), Nil).filterNot(_._1 == k).map(_._2).toSet
          def venues(segment: String) = segVenues.get(segment).fold(0)(_.size)
          def films(segment: String) = segFilms.get(segment).fold(0)(_.size)
          def chainVenues(segment: String) = segChains.get(segment).fold(0)(_.size)
          def knownDecoration(segment: String) = knownKeys(segment)
          def recordTitle(segment: String) = recordKeys(segment)
          def billedPlain(segment: String) = plainKeys(segment)
          def sibling(segment: String) = siblings(segment)
          def recordWordShare(tokens: Seq[String]) = tokens.count(recordWords).toDouble / math.max(1, tokens.size)
          def venuePrior(place: String) = {
            val (d, n) = priorCounts((x.venue, place))
            val (dd, nn) = own.filter(_._1 == place).fold((d, n)) { case (_, dec) => (d - (if (dec) 1 else 0), n - 1) }
            (dd + 1.0) / (nn + 2.0)
          }
          val directorWords = x.directors.flatMap(TitleContainment.tokens).filter(_.length >= 3).toSet
          val listingYear = x.year
          val originalTitle = x.originalTitle
          def search(segment: String) = None   // replaced below: the recorded answers are per country
        }
      val searchOf: (String, String) => Option[Seq[Seq[String]]] = (cc, text) => corpora.answers.get(cc).flatMap(_.search(text))
        .map(_.map(h => TitleContainment.tokens(h.title)))
      def segFeatures(k: (String, ListingKey), x: services.identity.Listing, segs: IndexedSeq[DecorationSegments.Segment], labels: Option[IndexedSeq[Boolean]]) = {
        segs.indices.map { i =>
          val own = labels.map(l => DecorationSegments.place(segs, i) -> l(i))
          val ctx = contextOf(k, x, own)
          val withSearch = new DecorationSegments.Context {
            export ctx.{venues, films, chainVenues, knownDecoration, recordTitle, billedPlain, sibling, recordWordShare, venuePrior, directorWords,
              listingYear, originalTitle}
            def search(segment: String) = searchOf(k._1, segs(i).text)
          }
          DecorationSegments.features(segs, i, withSearch)
        }
      }
      val trainRows = segTrain.flatMap(x => segFeatures(x.key, byKey(x.key), x.segs, Some(x.labels)).zip(x.labels))
      val testFeatures = segTest.map(x => x -> segFeatures(x.key, byKey(x.key), x.segs, None))
      val unmatchedSegs = unmatched.flatMap { case (k, x) => DecorationSegments.segments(x.rawTitle).filterNot(DecorationSegments.billsSeveral)
        .map(segs => k -> segFeatures(k, x, segs, None).zip(segs)) }.toMap
      val variants = ("all" -> Set.empty[String]) +: DecorationSegments.Groups.map(g => s"-$g" -> Set(g))
      val segReport = variants.map { case (name, without) =>
        val m = DecorationSegments.fit(trainRows, without)
        val acc = testFeatures.count { case (x, fs) => m.inner(x.segs, fs) == x.e.aligned.inner }
        val accDec = testFeatures.filterNot(_._1.e.aligned.clean)
        val dec = accDec.count { case (x, fs) => m.inner(x.segs, fs) == x.e.aligned.inner }
        val props = proposals((k, _) => unmatchedSegs.get(k).map(pairs => m.inner(pairs.map(_._2).toIndexedSeq, i => pairs(i)._1)))
        tsv(out.resolve(s"proposals-segments$name.tsv"), "side\tdecoration\tfilms\tvenues\ttitles", props.map(_.productIterator.mkString("\t")))
        if (name == "all") Files.writeString(out.resolve("segment-model.json"), Json.prettyPrint(Json.obj("names" -> m.names, "weights" -> m.weights)) + "\n")
        (name, s"segments $name: held-out exact $acc/${testFeatures.size} (decorated $dec/${accDec.size}); ${props.size} proposal runs", m)
      }
      val tokenAcc = testFeatures.count { case (x, _) => model.inner(x.e.words, spread) == x.e.aligned.inner }
      val learnedAcc = testFeatures.count { case (x, _) => learnedExact(x.e) }
      val noStrip = testFeatures.count { case (x, _) => x.e.aligned.clean }
      val weights = segReport.head._3
      val top = DecorationSegments.Groups.map { g =>
        val names = DecorationSegments.features(IndexedSeq(DecorationSegments.Segment("x", Seq("x"), "edge", "edge")), 0, contextOf(byKey.keys.head, byKey.values.head))
          .filter(_.group == g).map(_.name).toSet
        s"  $g: " + weights.names.zip(weights.weights).filter(nw => names(nw._1)).sortBy(nw => -math.abs(nw._2)).take(6).map { case (n, w) => f"$n=$w%.2f" }.mkString(", ")
      }
      tsv(out.resolve("proposals.tsv"), "side\tdecoration\tfilms\tvenues\ttitles", tokenProposals.map(_.productIterator.mkString("\t")))
      val report = Seq(s"examples per country: ${counts.toSeq.sorted.map { case ((cc, k), n) => s"$cc $k $n" }.mkString(", ")}",
        s"train ${train.size} titles, held-out ${test.size} titles (venue split 1 in 5)") ++ byCountry ++
        Seq(s"token model: ${tokenProposals.size} proposal runs → ${out.resolve("proposals.tsv")}",
          s"segment examples: train ${segTrain.size}, held-out ${segTest.size} (titles whose film title starts or ends inside a segment, and double bills, left out)",
          s"on the segment held-out set: token model $tokenAcc, learned decorations $learnedAcc, no strip $noStrip of ${testFeatures.size}") ++
        segReport.map(_._2) ++ Seq("segment model (all) top weights by group:") ++ top
      report.foreach(println)
      Files.writeString(out.resolve("report.txt"), report.mkString("", "\n", "\n"))
      Files.writeString(out.resolve("model.json"), Json.prettyPrint(Json.obj("names" -> DecorationTokens.Names, "weights" -> model.weights)) + "\n")
    } finally storages.foreach(s => Try(s.close()))
  }

  private def tsv(path: Path, header: String, lines: Seq[String]): Unit =
    Files.writeString(path, (header +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
}
