package scripts

import models.Country
import services.identity.{IdentityResolver, Listing, OlderRefs, ResolverDecision}
import services.movies.{ListingKey, TitleNormalizer}
import tools.{IdentityJudgements, UnmatchedClusters}

import java.nio.file.Path
import java.util.Locale

/**
 * WHY a listing of the unmatched-cluster capture took (or did not take) its film, in one command —
 * `scripts/identity-slice <cc> --explain` (alias `scripts/identity-explain`)
 * compiles this at each git ref asked and runs it over ONE capture, so two refs' traces compare on one input:
 *
 *  - the listing as the venue bills it, and its venue page's facts as the capture recorded them;
 *  - the model's decision on its cluster as captured (the whole corpus's resolve), and the cluster re-resolved by THIS
 *    code over the capture's listings — family keys, the chain into its family, each candidate scored with its signals;
 *  - the agreement stage over the captured decision, by this code: the basis (agreed, poster, broadcast, filled, …), the
 *    fill rule and the guards it passed, and the film it took;
 *  - the verdict `identity-judgements.tsv` gives the take.
 *
 * Self-contained on purpose: the script copies this one file into an older ref's tree to explain that ref, so it reads
 * only [[UnmatchedClusters]] and [[IdentityResolver.explain]].
 *
 * {{{
 *   IdentityExplain <cc> [--venue S] [--title S] [--fixture <cc>.json.gz] [--labels identity-judgements.tsv] [--no-resolve] [--limit N]
 * }}}
 */
object IdentityExplain {

  final case class Options(country: String = "", venue: Option[String] = None, title: Option[String] = None,
                           fixture: Option[Path] = None, labels: Option[Path] = None, resolve: Boolean = true, limit: Int = 12)

  def parse(args: List[String], options: Options = Options()): Options = args match {
    case Nil                          => options
    case "--venue" :: v :: rest       => parse(rest, options.copy(venue = Some(v)))
    case "--title" :: v :: rest       => parse(rest, options.copy(title = Some(v)))
    case "--fixture" :: v :: rest     => parse(rest, options.copy(fixture = Some(Path.of(v))))
    case "--labels" :: v :: rest      => parse(rest, options.copy(labels = Some(Path.of(v))))
    case "--limit" :: v :: rest       => parse(rest, options.copy(limit = v.toInt))
    case "--no-resolve" :: rest       => parse(rest, options.copy(resolve = false))
    case cc :: rest if !cc.startsWith("--") && options.country.isEmpty => parse(rest, options.copy(country = cc.toLowerCase(Locale.ROOT)))
    case other :: _                   => sys.error(s"unknown argument $other")
  }

  /** Case and accents aside: "rückkehr" finds "Rückkehr", "ruckkehr" too. */
  def folded(text: String): String =
    java.text.Normalizer.normalize(text.toLowerCase(Locale.ROOT), java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "").replace("ł", "l")

  def selects(options: Options)(listing: Listing): Boolean =
    options.venue.forall(v => folded(listing.venue).contains(folded(v))) && options.title.forall(t => folded(listing.rawTitle).contains(folded(t)))

  def main(args: Array[String]): Unit = {
    val options = parse(args.toList)
    require(options.country.nonEmpty && (options.venue.isDefined || options.title.isDefined),
      "usage: IdentityExplain <cc> [--venue S] [--title S] [--fixture path] [--labels path] [--no-resolve]")
    val country = Country.all.find(_.code == options.country).getOrElse(sys.error(s"no country ${options.country}"))
    val fixture = options.fixture.getOrElse(UnmatchedClusters.fixturePath(country))
    val labels  = IdentityJudgements.labels(options.labels.getOrElse(IdentityJudgements.CheckedIn))
    val started = tools.Stopwatch.start()
    val capture = UnmatchedClusters.read(fixture)
    val chosen  = capture.listings.filter(selects(options))
    if (chosen.isEmpty) {
      println(s"no listing of $fixture matches — it holds the clusters the MODEL left unmatched; a listing the model took is " +
        "not captured (reproduce it over the full corpus: scripts/convergence-local.sh)")
      sys.exit(1)
    }
    val outcome   = UnmatchedClusters.replay(capture)
    val takes     = UnmatchedClusters.takes(capture, outcome).groupBy(t => (t.venue, t.rawTitle))
    val modelOf   = outcome.model.decisionOf
    val agreedOf  = outcome.agreed.decisionOf
    val wanted    = chosen.map(_.key).toSet
    val explained = if (options.resolve)
      IdentityResolver.explain(capture.listings, new UnmatchedClusters.Replay(capture), TitleNormalizer.forCountry(country))(wanted)
        .map(e => e.decision.members.filter(wanted).toSet -> e).toSeq
    else Nil
    val byCluster = chosen.groupBy(l => agreedOf.get(l.key).orElse(modelOf.get(l.key)).map(_.members.toSet).getOrElse(Set(l.key)))
    println(s"# ${country.code}: ${chosen.size} listing(s) in ${byCluster.size} cluster(s) of $fixture (${capture.listings.size} listings captured)")
    byCluster.toSeq.sortBy(_._2.map(_.sortKey).min).take(options.limit).foreach { case (_, listings) =>
      val first = listings.minBy(_.sortKey)
      println()
      println(s"== «${first.rawTitle}» — ${listings.map(_.venue).distinct.sorted.mkString(", ")}")
      // Listings billed alike (a chain's 40 venues) print once, with how many.
      listings.groupBy(facts(capture)).toSeq.sortBy(_._2.map(_.sortKey).min).foreach { case (lines, alike) =>
        val shownOne = alike.minBy(_.sortKey)
        println(s"listing ${shown(shownOne.key)}${if (alike.size > 1) s" (and ${alike.size - 1} more billed alike)" else ""}")
        shownOne.page.foreach(page => println(s"  page: $page"))
        lines.foreach(println)
      }
      val key = first.key
      modelOf.get(key).foreach(d => println(s"model (captured, whole corpus): ${d.render}"))
      explained.find(_._1.contains(key)).foreach { case (_, e) =>
        println("re-resolved by this code over the capture's listings:")
        e.render.foreach(line => println(s"  $line"))
        e.decision.trace.rulesOf(key).filter(_.nonEmpty) match {
          case Nil   => ()
          case rules => println(s"  rules: ${rules.mkString(", ")}")
        }
        e.decision.trace.nodes.get(key).foreach { node =>
          node.searched.foreach(s => println(s"  searched: $s"))
          node.refusals.foreach(r => println(s"  refused by ${r.rule}: ${r.why}${r.film.fold("")(f => s" (tmdb $f)")}${if (r.detail.nonEmpty) s" — ${r.detail}" else ""}"))
          node.blocker.foreach(b => println(s"  blocker: $b"))
        }
      }
      agreedOf.get(key) match {
        case Some(d) => println(s"agreement (this code): ${agreementLine(d)}"); d.explanation.foreach(line => println(s"    $line"))
        case None    => println("agreement (this code): no decision")
      }
      // One line per film taken and verdict: a chain's venues taking one film print once, with how many.
      val judged = listings.sortBy(_.sortKey).flatMap(l => takes.getOrElse((l.venue, l.rawTitle), Nil)).distinctBy(t => (t.venue, t.rawTitle, t.film)).map { take =>
        val about = labels.filter(_.covers(take)).map(l => s"${if (l.right) "right" else "wrong"} ${l.film} (${l.note})").distinct
        val verdict = UnmatchedClusters.verdict(take, labels) match { case Some(true) => "RIGHT"; case Some(false) => "WRONG"; case None => "UNJUDGED" }
        (s"${take.film} '${take.title}' via ${take.basis} — $verdict${if (about.nonEmpty) about.mkString(" [", "; ", "]") else ""}", take.venue)
      }
      judged.groupBy(_._1).toSeq.sortBy(_._1).foreach { case (line, venues) =>
        val where = venues.map(_._2).distinct
        println(s"TAKE ${if (where.size == 1) where.head else s"at ${where.size} venues"}: $line")
      }
      if (listings.forall(l => takes.get((l.venue, l.rawTitle)).isEmpty)) println("TAKE none: the cluster stays without a film")
    }
    if (byCluster.size > options.limit) println(s"\n… ${byCluster.size - options.limit} more cluster(s); narrow with --venue/--title or raise --limit")
    System.err.println(f"[identity-explain] ${started.seconds}%.1fs")
  }

  /** A listing's key as one line: its fields' separators (NUL) as " · ". */
  def shown(key: ListingKey): String = ListingKey.serialised(key).replace('\u0000', '·').trim

  /** What a listing bills and its venue page says, as printed: the same lines for listings billed alike. */
  def facts(capture: UnmatchedClusters.Capture)(l: Listing): Seq[String] = {
    def names(people: Seq[String]) = if (people.isEmpty) "-" else people.mkString(", ")
    Seq(s"  billed: title '${l.title}', clean '${l.cleanTitle}'${l.searchTitle.fold("")(s => s", search '$s'")}${OlderRefs.optionalText(l, "properTitle").fold("")(s => s", proper '$s'")}" +
      s"${l.originalTitle.fold("")(o => s", original '$o'")}; year ${l.year.getOrElse("-")}, runtime ${l.runtime.getOrElse("-")}, " +
      s"directors ${names(l.directors)}${if (l.countries.nonEmpty) s", countries ${l.countries.mkString(",")}" else ""}" +
      s"${if (l.catalogueIds.nonEmpty) s", catalogue ${l.catalogueIds.map(c => s"${c.source}:${c.id}").mkString(",")}" else ""}") ++
      capture.details.get(ListingKey.serialised(l.key)).map {
        case Some(d) => s"  venue page: year ${d.year.getOrElse("-")}, runtime ${d.runtime.getOrElse("-")}, directors ${names(d.directors)}" +
          s"${d.originalTitle.fold("")(o => s", original '$o'")}${d.synopsis.fold("")(s => s"\n    synopsis: ${s.take(240)}")}"
        case None    => "  venue page: read, no facts"
      }
  }

  def agreementLine(d: ResolverDecision): String =
    s"${d.film.fold(d.fallback.fold("no film")(f => s"${f.source}:${f.id}"))(id => s"tmdb $id")} (${ResolverDecision.percent(d.confidence)}, ${d.basis})"
}
