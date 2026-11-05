package services.identity

import models.Country
import org.bson.BsonDocument
import org.mongodb.scala.{MongoClient, MongoDatabase, ObservableFuture}
import services.identity.adapters.{IdentityHistoryReplay, MongoIdentityHistory, MongoTmdbDocuments}
import services.movies.{ListingKey, TitleNormalizer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Clock, Instant}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * ONE SIDE OF A WHAT-IF (`scripts/identity-what-if`): this checkout's model over the LOCAL prod mirror's corpus as its
 * history held it at `--at` (`mongodb://127.0.0.1:28017`, read-only; listings, venue pages and the TMDB store replayed by
 * [[IdentityHistoryReplay]]), so a base and a head built from two refs resolve the very same snapshot.
 *
 *  - BASE (no `--select`): every family, resolved against the corpus's context ([[IdentityResolver.resolveRegion]]).
 *    Writes `takes.tsv` (each listing's family, take and the rule ids that decided it: [[DecisionTrace.rulesOf]] and the
 *    stage's), `families.tsv` (each family's block keys) and `prod.tsv` (prod's served take, as the mirror holds it now).
 *  - HEAD (`--select FILE --families FILE`): only the base families the change can reach (the script's selection by
 *    rule ids, every listing key a line), re-resolved by this code against the corpus's context — group by group on
 *    `--threads`, widened ([[affected]]) while a re-resolved family carries a block key of another group or of a base
 *    family outside. Writes `takes.tsv` for the listings re-resolved, and `region.tsv` (how it widened). `--whole` also
 *    resolves every family (`takes-whole.tsv`): what `--verify` checks the selection against.
 *
 * Keys are [[ListingKey.serialised]] with its NULs as U+001E; a list field's items are U+001F-separated.
 *
 *   worker/Test/runMain services.identity.WhatIfResolve <cc> --at ISO --out DIR [--select FILE --families FILE [--whole]] [--threads 6]
 *
 * Self-contained on purpose: the script drops this file (and tools/MemoLookups.scala) into a ref from before it.
 */
object WhatIfResolve {

  /** A base family as `families.tsv` and `takes.tsv` hold it: its members' keys and the keys it blocks under. */
  final case class Family(id: String, members: Set[String], blockKeys: Set[String])

  /** One family of a re-resolve: its members' keys and its block keys. */
  final case class Resolved(members: Set[String], blockKeys: Set[String])

  /** How a selection widened: the groups re-resolved last, each group's families, and the rounds it took. */
  final case class Region(groups: Seq[Set[String]], resolved: Seq[Seq[Resolved]], rounds: Int) {
    def listings: Set[String] = groups.flatten.toSet
  }

  /**
   * The re-resolve of `selected` (keys of whole base families): each base family holding one is a group, every group
   * resolved on its own (`resolve`, many at once); then groups whose families share a block key are merged, and a base
   * family outside whose block key a group's family carries is joined to it — the families a whole resolve would have
   * merged — and only the groups that changed are resolved again, until none does.
   */
  def affected(selected: Set[String], base: Seq[Family], resolve: Seq[Set[String]] => Seq[Seq[Resolved]]): Region = {
    val familyOf  = base.flatMap(f => f.members.map(_ -> f)).toMap
    val byKey     = base.flatMap(f => f.blockKeys.map(_ -> f)).groupMap(_._1)(_._2)
    var groups    = selected.toSeq.flatMap(familyOf.get).distinct.map(_.members) ++
      (selected -- familyOf.keySet).toSeq.sorted.map(Set(_))
    var results   = Map.empty[Set[String], Seq[Resolved]]
    var rounds    = 0
    var changed   = true
    while (changed) {
      rounds += 1
      val fresh = groups.filterNot(results.contains)
      results = results.filter(r => groups.contains(r._1)) ++ fresh.zip(resolve(fresh))
      // a group's block keys reach other groups and base families: merge everything reachable, transitively
      val keysOf  = groups.map(g => g -> results(g).flatMap(_.blockKeys).toSet).toMap
      val holders = groups.flatMap(g => keysOf(g).map(_ -> g)).groupMap(_._1)(_._2)
      val merged  = scala.collection.mutable.ArrayBuffer.empty[Set[String]]
      val seen    = scala.collection.mutable.Set.empty[Set[String]]
      groups.foreach { start =>
        if (!seen(start)) {
          var members = Set.empty[String]
          val queue   = scala.collection.mutable.Queue(start)
          seen += start
          while (queue.nonEmpty) {
            val g = queue.dequeue()
            members ++= g
            keysOf(g).foreach { key =>
              holders.getOrElse(key, Nil).filterNot(seen).foreach { other => seen += other; queue.enqueue(other) }
              byKey.getOrElse(key, Nil).foreach(f => members ++= f.members)
            }
          }
          merged += members
        }
      }
      changed = merged.toSet != groups.toSet
      groups  = merged.toSeq
    }
    Region(groups, groups.map(results), rounds)
  }

  final case class Corpus(listings: Seq[Listing], lookups: IdentityLookups)

  /** Every live ask refused: a gap stays the gap the mirror's store holds, nothing is fetched. */
  private object Refused extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]       = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }

  /** The country's corpus as the mirror's history held it at `at`: listings, venue pages and TMDB documents. */
  def corpusAt(db: MongoDatabase, country: Country, at: Instant, normalizer: TitleNormalizer, clock: Clock): Corpus = {
    val replay   = IdentityHistoryReplay.from(new MongoIdentityHistory(db, new services.TtlIndexMismatches), at.toEpochMilli)
    val rows     = tools.ProdListings.read(db, tools.CountryScrapeCorpus.cinemasOf(country).toSet)
    val listings = replay.listings(ModelRead.corpus(rows.map(r => r.cinema -> r.films), normalizer))
    val pages    = replay.pages(Await.result(db.getCollection[BsonDocument](services.venuepages.MongoVenuePageStore.Collection).find()
      .batchSize(tools.MongoReplies.Default).toFuture(), 5.minutes))
    val lookups  = tools.ProdLookupInputs.lookups(replay.documents(new MongoTmdbDocuments(db)), pages,
      tools.ProdLookupInputs.enrichersOf(country, clock), Refused, country, clock)
    Corpus(listings.sorted.distinctBy(_.key), new tools.MemoLookups(lookups))
  }

  /** The keys a selection file lists, one a line, each WHOLE: a key may end in its separator (a page-less listing
   *  with no year or director), which a trim would cut off. */
  def selection(lines: Seq[String]): Set[String] = lines.filter(_.nonEmpty).toSet

  /** A listing's key as the files hold it: [[ListingKey.serialised]], its NULs as U+001E and a line break or tab a raw
   *  title carries escaped ("„Skrzypek na dachu”\nMusical wszech czasów…"), so a key is one field of one line. */
  def keyOf(key: ListingKey): String =
    ListingKey.serialised(key).replace('\u0000', '\u001e').replace("\\", "\\\\").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")
  private def clean(text: String): String = text.replaceAll("[\t\r\n]", " ")
  private def list(items: Iterable[String]): String = items.toSeq.sorted.map(clean).mkString("\u001f")
  private def items(field: String): Set[String] = if (field.isEmpty) Set.empty else field.split('\u001f').toSet
  private def tsv(path: Path, header: String, rows: Iterable[Seq[String]]): Unit =
    Files.writeString(path, (header +: rows.map(_.mkString("\t")).toSeq).mkString("", "\n", "\n"), StandardCharsets.UTF_8)

  /** Each listing's row of `takes.tsv`: key, venue, raw title, family, take (`pending` when none), the rule ids. */
  private def takeRows(families: Seq[IdentityResolver.RegionFamily], byKey: Map[ListingKey, Listing]): Seq[Seq[String]] =
    families.flatMap { family =>
      val id      = keyOf(family.listings.minBy(keyOf))
      val decided = family.decisions.flatMap(d => d.members.map(_ -> d)).toMap
      family.listings.toSeq.sortBy(keyOf).filter(byKey.contains).map { key =>
        val decision = decided.get(key)
        Seq(keyOf(key), clean(key.venue), clean(key.rawTitle), id, decision.flatMap(PopulationMonotonicity.takeOf).getOrElse("pending"),
          list(decision.toSeq.flatMap(d => d.trace.rulesOf(key) ++ d.trace.stage).distinct))
      }
    }

  def main(args: Array[String]): Unit = {
    val clock   = Clock.systemUTC()
    val cc      = args.headOption.getOrElse(sys.error("a country code"))
    val country = Country.all.find(_.code == cc).getOrElse(sys.error(s"no country $cc"))
    def opt(name: String) = args.toList.sliding(2).collectFirst { case List(`name`, value) => value }
    val at      = Instant.parse(opt("--at").getOrElse(sys.error("--at ISO: the mirror snapshot both sides resolve")))
    val out     = Path.of(opt("--out").getOrElse(sys.error("--out DIR")))
    val threads = opt("--threads").fold(6)(_.toInt)
    val select  = opt("--select").map(Path.of(_))
    val whole   = args.contains("--whole")
    val uri     = "mongodb://127.0.0.1:28017/?directConnection=true"
    val client  = MongoClient(uri)
    Files.createDirectories(out)
    val timings = scala.collection.mutable.ArrayBuffer.empty[Seq[String]]
    def timed[A](phase: String)(body: => A): A = {
      val started = tools.Stopwatch.start()
      val result  = body
      timings += Seq(phase, f"${started.seconds}%.1f")
      System.err.println(f"[what-if] $cc $phase: ${started.seconds}%.1fs")
      result
    }
    try {
      val db         = client.getDatabase(services.MongoConnection.mirrorDbFor(country.mongoDb))
      val normalizer = TitleNormalizer.forCountry(country)
      val inputs     = ResolveInputs.of(new MongoPinStore(Some(db)).all())
      val corpus     = timed("corpus")(corpusAt(db, country, at, normalizer, clock))
      val byKey      = corpus.listings.map(l => l.key -> l).toMap
      val bySerial   = corpus.listings.map(l => keyOf(l.key) -> l).toMap
      val context    = timed("context")(new CandidateGeneration(corpus.listings, corpus.lookups, normalizer, inputs.pins, inputs.decorations,
        lazyLookups = false).context)
      def region(listings: Iterable[Listing]) = IdentityResolver.resolveRegion(listings, corpus.lookups, normalizer, inputs.calibration,
        inputs.pins, inputs.decorations, context)
      def all() = region(corpus.listings)
      select match {
        case None =>
          val families = timed("resolve-all")(all())
          tsv(out.resolve("takes.tsv"), "key\tvenue\trawTitle\tfamily\ttake\trules", takeRows(families, byKey))
          tsv(out.resolve("families.tsv"), "family\tblockKeys", families.map(f => Seq(keyOf(f.listings.minBy(keyOf)), list(f.blockKeys))))
          val prod  = tools.ProdMirror.open(uri, country)
          val sides = timed("prod")(try prod.sides(corpus.listings.map(_.key)) finally prod.close())
          tsv(out.resolve("prod.tsv"), "key\tprod\tbasis", corpus.listings.map { l =>
            val (film, basis) = sides.get(l.key).fold(("missing", "-"))(_.take)
            Seq(keyOf(l.key), film, clean(basis))
          })
        case Some(file) =>
          val selected = selection(Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toSeq)
          val familiesFile = Path.of(opt("--families").getOrElse(sys.error("--families FILE: the base's families.tsv")))
          val baseTakes    = Path.of(opt("--base-takes").getOrElse(familiesFile.resolveSibling("takes.tsv").toString))
          val blockKeys    = Files.readAllLines(familiesFile, StandardCharsets.UTF_8).asScala.drop(1).map(_.split("\t", -1))
            .map(cols => cols(0) -> items(cols(1))).toMap
          val members      = Files.readAllLines(baseTakes, StandardCharsets.UTF_8).asScala.drop(1).map(_.split("\t", -1))
            .map(cols => cols(3) -> cols(0)).groupMap(_._1)(_._2)
          val base = blockKeys.toSeq.map { case (id, keys) => Family(id, members.getOrElse(id, Nil).toSet, keys) }
          val pool = tools.DaemonExecutors.boundedEC("what-if", threads)
          val decided = new java.util.concurrent.ConcurrentHashMap[Set[String], Seq[IdentityResolver.RegionFamily]]()
          val result = try timed("resolve-selected")(affected(selected.filter(bySerial.contains), base, groups =>
            groups.map(g => pool.submit(() => { val r = region(g.toSeq.flatMap(bySerial.get)); decided.put(g, r); r })).map(_.get)
              .map(_.map(f => Resolved(f.listings.map(keyOf), f.blockKeys))))) finally { val _ = pool.shutdownNow() }
          tsv(out.resolve("takes.tsv"), "key\tvenue\trawTitle\tfamily\ttake\trules", result.groups.flatMap(g => takeRows(decided.get(g), byKey)))
          tsv(out.resolve("region.tsv"), "selected\tre-resolved\tgroups\trounds",
            Seq(Seq(selected.size.toString, result.listings.size.toString, result.groups.size.toString, result.rounds.toString)))
          if (whole) {
            val families = timed("resolve-all")(all())
            tsv(out.resolve("takes-whole.tsv"), "key\tvenue\trawTitle\tfamily\ttake\trules", takeRows(families, byKey))
          }
      }
      tsv(out.resolve("timings.tsv"), "phase\tseconds", timings.toSeq :+ Seq("listings", corpus.listings.size.toString))
    } finally client.close()
  }
}
