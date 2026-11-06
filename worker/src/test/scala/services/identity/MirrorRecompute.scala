package services.identity

import models.Country
import org.mongodb.scala.{MongoClient, MongoDatabase}
import services.movies.{ListingKey, TitleNormalizer}

import java.time.{Clock, Duration, Instant}

/**
 * AFFECTED-ONLY RECOMPUTE on the LOCAL prod mirror (`mongodb://127.0.0.1:28017`, read-only; run
 * `scripts/local-mirror/freshness <cc>` first), keyed by the family index ([[FamilyIndex]]): the country's corpus as the
 * mirror's history held it at `--from` (listings, venue pages, TMDB store: [[IdentityHistoryReplay]]) indexed; the
 * corpus at `--to` read; the families whose index no longer stands there ([[FamilyIndex.Entry.stale]]), with every
 * listing new since, re-resolved against the `--to` corpus's context -- widened while a re-resolved family shares a
 * block key with one that was not -- and every other family's take kept. The model's take; the agreement stage after it
 * is not recomputed here.
 *
 * Checked against a WHOLE resolve at `--to` (the recompute is sound when they agree on every listing) and written as a
 * takes diff: each listing whose take moved from `--from` to `--to`, with what moved in its family's index, and whether
 * prod's model (`identity_model_families`, as the mirror holds it now) takes the same.
 *
 *   worker/Test/runMain services.identity.MirrorRecompute <cc> [--from ISO|--hours-back 6] [--to ISO] [--threads 6] [--out DIR]
 */
object MirrorRecompute {

  final case class Corpus(listings: Seq[Listing], lookups: IdentityLookups)

  /** The mirror's corpus at `at` ([[MirrorWorld]]), its lookups dated by that moment. */
  def corpusAt(db: MongoDatabase, country: Country, at: Instant, normalizer: TitleNormalizer): Corpus = {
    val world   = MirrorWorld.at(db, country, at, normalizer)
    val lookups = tools.ProdLookupInputs.lookups(world.documents, world.pages, tools.ProdLookupInputs.enrichersOf(country, world.clock),
      scripts.IdentityHistoryTools.Refused, country, world.clock)
    Corpus(world.listings, new tools.MemoLookups(lookups))
  }

  /** The recompute: each listing's take at `to`, which families were re-resolved, and why each was. */
  final case class Recomputed(takes: Map[ListingKey, Option[String]], recomputed: Seq[Set[ListingKey]], why: Map[ListingKey, Seq[String]],
                              rounds: Int)

  def recompute(from: FamilyIndex, to: Corpus, normalizer: TitleNormalizer, inputs: ResolveInputs): Recomputed = {
    val generation = new CandidateGeneration(to.listings, to.lookups, normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val byKey      = to.listings.map(l => l.key -> l).toMap
    val now        = FamilyIndex.Now(generation.context, to.lookups, byKey.get, inputs.pins, FamilyIndex.blockKeyHolders(from))
    val stale      = from.stale(now)
    val why        = stale.flatMap { case (entry, reasons) => entry.members.keys.map(_ -> reasons) }.toMap
    val known      = from.entries.flatMap(_.members.keys).toSet
    val newcomers  = to.listings.map(_.key).filterNot(known)
    // the affected listings: the stale families' present members and every newcomer, widened to each family whose block
    // keys the re-resolved families now carry, until none is added
    var affected   = stale.flatMap(_._1.members.keys).filter(byKey.contains).toSet ++ newcomers
    var regions    = Seq.empty[IdentityResolver.RegionFamily]
    var rounds     = 0
    var widened    = true
    while (widened) {
      rounds += 1
      regions = IdentityResolver.resolveRegion(affected.toSeq.map(byKey), to.lookups, normalizer, inputs.calibration, inputs.pins,
        inputs.decorations, generation.context)
      val keys   = regions.flatMap(_.blockKeys).toSet
      val joined = from.entries.filter(e => !e.members.keySet.subsetOf(affected) && e.blockKeys.exists(keys)).flatMap(_.members.keys).filter(byKey.contains)
      widened    = joined.exists(k => !affected(k))
      affected ++= joined
    }
    val fresh = regions.flatMap(_.decisions).flatMap(d => d.members.map(_ -> PopulationMonotonicity.takeOf(d))).toMap
    val kept  = from.entries.filterNot(_.members.keySet.exists(affected)).flatMap(_.takes).filter(e => byKey.contains(e._1)).toMap
    Recomputed(kept ++ fresh, regions.map(_.listings), why ++ newcomers.map(_ -> Seq("new listing")), rounds)
  }

  def main(args: Array[String]): Unit = {
    val clock   = Clock.systemUTC()
    val cc      = args.headOption.getOrElse(sys.error("a country code"))
    val country = Country.all.find(_.code == cc).getOrElse(sys.error(s"no country $cc"))
    def opt(name: String) = args.toList.sliding(2).collectFirst { case List(`name`, value) => value }
    val to      = opt("--to").map(Instant.parse).getOrElse(clock.instant())
    val from    = opt("--from").map(Instant.parse).getOrElse(to.minus(Duration.ofHours(opt("--hours-back").fold(6L)(_.toLong))))
    val threads = opt("--threads").fold(6)(_.toInt)
    val out     = java.nio.file.Path.of(opt("--out").getOrElse("target/mirror-recompute"))
    val uri     = "mongodb://127.0.0.1:28017/?directConnection=true"
    val client  = MongoClient(uri)
    try {
      val db         = client.getDatabase(services.MongoConnection.mirrorDbFor(country.mongoDb))
      val normalizer = TitleNormalizer.forCountry(country)
      val inputs     = ResolveInputs.of(new MongoPinStore(Some(db)).all())
      val started    = tools.Stopwatch.start()
      def seconds    = f"${started.seconds}%.0fs"
      val before     = corpusAt(db, country, from, normalizer)
      val index      = FamilyIndex.build(before.listings, before.lookups, normalizer, inputs, threads)
      System.err.println(s"[mirror-recompute] $cc at $from: ${before.listings.size} listings, ${index.index.entries.size} families indexed ($seconds)")
      val after      = corpusAt(db, country, to, normalizer)
      val recomputing = tools.Stopwatch.start()
      val result     = recompute(index.index, after, normalizer, inputs)
      val recomputeSeconds = recomputing.seconds
      System.err.println(f"[mirror-recompute] $cc at $to: ${after.listings.size} listings; ${result.recomputed.size} families re-resolved " +
        f"(${result.recomputed.map(_.size).sum} listings, ${result.rounds} round(s)) in $recomputeSeconds%.0fs")
      val resolving  = tools.Stopwatch.start()
      val whole      = IdentityResolver.resolve(after.listings, after.lookups, normalizer, inputs)
      val wholeSeconds = resolving.seconds
      val wholeTakes = after.listings.map(l => l.key -> whole.decisionOf.get(l.key).flatMap(PopulationMonotonicity.takeOf)).toMap
      val unsound    = after.listings.map(_.key).filter(k => result.takes.get(k).flatten != wholeTakes(k))
      val prod       = tools.ProdMirror.open(uri, country)
      val sides      = try prod.sides(after.listings.map(_.key)) finally prod.close()
      val was        = index.index.entries.flatMap(_.takes).toMap
      val moved      = after.listings.map(_.key).filter(k => was.get(k).flatten != result.takes.get(k).flatten)
      def show(t: Option[String]) = t.getOrElse("pending")
      java.nio.file.Files.createDirectories(out)
      val lines = moved.map { k =>
        val prodTake = sides.get(k).map(_.take._1).getOrElse("-")
        Seq(cc, k.venue, k.rawTitle, was.get(k).flatten.fold("absent")(identity), show(result.takes.get(k).flatten), prodTake,
          result.why.getOrElse(k, Nil).take(3).mkString("; ")).mkString("\t")
      }
      java.nio.file.Files.writeString(out.resolve(s"takes-diff-$cc.tsv"),
        ("country\tvenue\ttitle\tfrom\tto\tprod\twhy" +: lines).mkString("", "\n", "\n"))
      println(f"## $cc $from -> $to: ${after.listings.size} listings, ${index.index.entries.size} families; " +
        f"${result.recomputed.size} re-resolved (${result.recomputed.map(_.size).sum} listings) in $recomputeSeconds%.0fs vs whole resolve " +
        f"$wholeSeconds%.0fs; ${unsound.size} listing(s) where the recompute and the whole resolve differ; ${moved.size} take(s) moved " +
        f"(${out.resolve(s"takes-diff-$cc.tsv")})")
      // which fields moved under the members re-read as moved: what a member digest should, or should not, hold
      val beforeByKey = before.listings.map(l => l.key -> l).toMap
      val fieldsMoved = after.listings.flatMap(l => beforeByKey.get(l.key).filter(b => FamilyIndex.modelDigest(b) != FamilyIndex.modelDigest(l)).toSeq
        .flatMap(b => b.productElementNames.zip(b.productIterator.zip(l.productIterator)).collect { case (name, (x, y)) if x != y => name }))
        .groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortBy(-_._2)
      println(s"  members moved, by field: ${fieldsMoved.take(8).map { case (k, n) => s"$k $n" }.mkString(", ")}")
      val kinds = result.why.values.flatten.map(_.takeWhile(_ != '|')).groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortBy(-_._2)
      println(s"  why re-resolved (by read kind, listings): ${kinds.take(8).map { case (k, n) => s"$k $n" }.mkString(", ")}")
      val taken = result.takes.values.groupMapReduce(t => t.fold("pending")(f => if (f == TakeFlips.NoFilm) "none" else "film"))(_ => 1)(_ + _)
      println(s"  takes at --to: ${taken.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}")
      val compared = after.listings.map(_.key).flatMap(k => sides.get(k).map(side => k -> side.take._1)).filter(_._2 != "missing")
      val agree    = compared.count { case (k, prodTake) => result.takes.get(k).flatten.contains(prodTake) }
      println(s"  against prod's served take (the mirror's identity_model_families, corrections and verdicts): ${agree} of ${compared.size} agree")
      unsound.take(20).foreach(k => println(s"  UNSOUND ${k.venue} | ${k.rawTitle}: recompute ${show(result.takes.get(k).flatten)}, whole ${show(wholeTakes(k))}"))
      lines.take(20).foreach(l => println(s"  $l"))
    } finally client.close()
  }
}
