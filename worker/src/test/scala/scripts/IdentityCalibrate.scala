package scripts

import models.Country
import play.api.libs.json.{JsArray, JsObject, Json}
import services.identity.{IdentityCalibration, IdentityMeasures}
import services.identity.IdentityCalibration.{Bin, Calibration, CannotLinkRule, Condition, ScopeModel, SignalWeights, Threshold}
import services.identity.IdentityMeasures.{Category, Film, Measure, Missing, Number}
import scripts.IdentityCalibrationData.*

import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPOutputStream
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * Derives the identity resolver's weights, calibration, thresholds and cannot-link rules from
 * EVIDENCE, and writes them as data (`common/src/main/resources/identity-weights.json`) beside the
 * labelled set they were fitted on (`test/resources/fixtures/identity/identity-labels.json.gz`).
 * Run through `scripts/identity-calibrate.sh`; docs/design/identity-resolver.md §calibration
 * reports what it measured.
 *
 * LABELS. Production's tmdbId for a listing is a PROPOSAL, not a label: some are wrong. It becomes
 * a positive only when the listing's own evidence, independent of every rule being calibrated,
 * corroborates it on at least `k` (default 2) of: the venue's year within ±1, a director TMDB
 * credits, the venue's original title naming it, and two or more other venues whose own year or
 * director back it — and nothing it published denies it. The IMDb cross-link is NOT a corroborator:
 * production's imdbId is TMDB's own external id for 99.9% of films (measured), so it is not
 * independent. Two or more denials (year off by 2+, a director nobody in the credits, an original
 * title sharing no word) mark the proposal CONTRADICTED: listed for healing, never used. The rest is
 * unlabelled and unused. Negatives are the other films the listing's own title search returned
 * (hard negatives: remakes, namesakes), for listings whose positive is corroborated.
 *
 * NO CIRCULARITY. A signal's weight is measured on labels that do not use that signal (leave one
 * corroborator out): the year's table is fitted on films corroborated by director, original title
 * and venues only. That is exactly naive Bayes' own assumption (signals independent given the film),
 * so the tables are unbiased under the model they feed. A cannot-link rule's false-veto rate is
 * measured on labels that use none of the rule's signals.
 *
 * SPLITS. Listings are grouped into families (connected by title key or by production film) and a
 * family goes wholly to one split by a hash of its smallest listing key: 50% train (weights, bins,
 * rules), 20% calibration (isotonic map, thresholds), 30% held out (every reported number). The
 * resolver's benchmark must use only the held-out split.
 */
object IdentityCalibrate {

  final case class Config(corpora: Path, fixtures: Path, hardClusters: Path, prod: Option[Path], weightsOut: Path,
                          labelsOut: Path, reportDir: Path, epsilon: Option[Double], countries: Seq[String], version: String)

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    def path(k: String) = opts.get(k).map(Paths.get(_))
    val cfg = Config(
      corpora      = path("corpora").getOrElse(sys.error("--corpora <dir of cinema-scrapes-<cc>.json.gz>")),
      fixtures     = path("fixtures").getOrElse(sys.error("--fixtures <dir of enrichment-<cc>/ trees>")),
      hardClusters = path("hard-clusters").getOrElse(Paths.get("test/resources/fixtures/corpus")),
      prod         = path("prod"),
      weightsOut   = path("weights").getOrElse(Paths.get("common/src/main/resources/identity-weights.json")),
      labelsOut    = path("labels").getOrElse(Paths.get("test/resources/fixtures/identity/identity-labels.json.gz")),
      reportDir    = path("report").getOrElse(Paths.get("target/identity-calibration")),
      // Default "certified": a cannot-link must never have fired on a same-film unit. A number
      // relaxes that to a bound on the false-veto rate.
      epsilon      = opts.get("epsilon").filter(_ != "certified").map(_.toDouble),
      countries    = opts.get("countries").map(_.split(",").toSeq).getOrElse(Country.all.map(_.code)),
      version      = opts.getOrElse("version", "unversioned"))
    run(cfg)
  }

  // ── labels ────────────────────────────────────────────────────────────────────────────

  val Corroborators: Seq[String] = Seq("year", "director", "originalTitle", "venues")

  /** What a listing's own evidence says about the film production filed it under. */
  final case class Evidence(tmdbId: Int, agree: Set[String], deny: Set[String]) {
    def positive(excluded: Set[String], k: Int): Boolean = (agree -- excluded).size >= k && (deny -- excluded).isEmpty
    def contradicted: Boolean = deny.size >= 2
  }

  /** Which corroborator a signal would read, so its table is fitted on labels without it. */
  def corroboratorOf(signal: String): Option[String] = signal match {
    case "year.delta" | "year.distance"  => Some("year")
    case "director"                      => Some("director")
    case "originalTitle"                 => Some("originalTitle")
    case "venues.corroborating"          => Some("venues")
    case "runtime.delta"                 => Some("runtime")
    case _                               => None
  }

  private def ownAgreement(m: Map[String, Measure]): (Set[String], Set[String]) = {
    val agree = mutable.Set.empty[String]; val deny = mutable.Set.empty[String]
    number(m.get("year.distance")).foreach(d => if (d <= 1) agree += "year" else deny += "year")
    category(m.get("director")).foreach { c => if (c == "same_person") agree += "director" else if (c == "different") deny += "director" }
    category(m.get("originalTitle")).foreach { c => if (c == "match") agree += "originalTitle" else if (c == "disjoint") deny += "originalTitle" }
    (agree.toSet, deny.toSet)
  }

  // ── one country's pairs ──────────────────────────────────────────────────────────────

  /** One listing-film pair: the listing, the candidate, its measurements. */
  final case class LfPair(obs: Int, tmdbId: Int, measures: Map[String, Measure])
  /** One listing-listing pair. */
  final case class LlPair(a: Int, b: Int, measures: Map[String, Measure])

  final case class CountryData(code: String, obs: IndexedSeq[Obs], evidence: Map[Int, Evidence], lf: Seq[LfPair],
                               ll: Seq[LlPair], prodFilms: Map[String, ProdFilm], details: Int => Option[Details],
                               stats: Map[String, Int])

  private def load(cfg: Config, country: Country, startIdx: Int): CountryData = {
    val cc = country.code
    val corpora = Seq(
      "full" -> cfg.corpora.resolve(s"cinema-scrapes-$cc.json.gz"),
      "hard-clusters" -> cfg.hardClusters.resolve(s"cinema-scrapes-hard-clusters-$cc.json.gz")).filter(p => Files.exists(p._2))
    val prod = cfg.prod.map(d => prodSnapshot(d.resolve(s"prod-${country.mongoDb}.jsonl"))).getOrElse(ProdSnapshot(Map.empty, Nil))
    val obs = listings(country, corpora, prod, startIdx).toIndexedSeq
    val answers = new TmdbAnswers(Seq(cfg.fixtures.resolve(s"enrichment-$cc")).filter(Files.isDirectory(_)),
      responsesFile(cfg.hardClusters.resolve(s"hard-clusters-responses-$cc.json.gz")), languageOf(country))
    println(s"[$cc] ${obs.size} listings (${obs.groupMapReduce(_.source)(_ => 1)(_ + _)}), ${prod.films.size} production films")

    // Candidate pools: every film the listing's own title queries returned, best rank kept.
    val pools: IndexedSeq[Map[Int, (Option[Int], Film)]] = obs.map { o =>
      val hits = queries(o.listing).flatMap(q => answers.search(q).toSeq.flatMap(_.zipWithIndex))
      val ranked = hits.groupMapReduce(_._1.id)(h => h)((a, b) => if (a._2 <= b._2) a else b)
      val fromSearch = ranked.map { case (id, (hit, rank)) =>
        id -> (Some(rank + 1), answers.details(id).map(_.film).getOrElse(
          Film(hit.title, hit.originalTitle, Nil, hit.year, None, None, None, Some(hit.popularity))))
      }
      // Films the recorded answers know under one of the listing's title shapes, found by no
      // recorded search: remakes and namesakes the family's other lookups surfaced.
      val known = queries(o.listing).map(IdentityMeasures.key).flatMap(answers.byTitleKey.getOrElse(_, Nil)).distinct
        .filterNot(fromSearch.contains).flatMap(id => answers.details(id).map(d => id -> (Option.empty[Int], d.film))).toMap
      val proposed = o.prodFilm.flatMap(prod.films.get).flatMap(_.tmdbId).flatMap(id => answers.details(id).map(d => id -> d.film))
      val pool = fromSearch ++ known
      proposed.fold(pool)(p => if (pool.contains(p._1)) pool else pool + (p._1 -> (None, p._2)))
    }

    // Venue co-occurrence: per title group, which venues' own facts back each candidate.
    val groups: Map[String, IndexedSeq[Int]] = obs.indices.groupBy(i => IdentityMeasures.key(obs(i).listing.title))
    val backers = mutable.HashMap.empty[(String, Int), Set[String]]
    def backing(group: String, film: Int, f: Film): Set[String] = backers.getOrElseUpdate((group, film),
      groups(group).iterator.map(obs).filter { o =>
        o.listing.statedYear.exists(y => f.year.contains(y)) ||
          f.directors.exists(ds => IdentityMeasures.directorRelation(o.listing.directors, ds) == Category("same_person"))
      }.map(_.venue).toSet)

    val lf = Seq.newBuilder[LfPair]
    obs.indices.foreach { i =>
      val o = obs(i); val pool = pools(i); val g = IdentityMeasures.key(o.listing.title)
      val closeTitles = pool.map { case (id, (_, f)) => id -> IdentityMeasures.titleRelation(o.listing, f).value }
      val close = closeTitles.count { case (_, r) => r == "exact" || r == "original" || r == "alternative" }
      pool.foreach { case (id, (rank, f)) =>
        val own = closeTitles(id)
        val rivals = close - (if (own == "exact" || own == "original" || own == "alternative") 1 else 0)
        val venues = (backing(g, id, f) - o.venue).size
        lf += LfPair(o.idx, id, IdentityMeasures.listingFilm(o.listing, f, rank, rivals, venues))
      }
    }
    val lfPairs = lf.result()

    // Corroboration of production's proposal, from the listing's own evidence and other venues'.
    val proposal: Map[Int, Int] = obs.flatMap(o => o.prodFilm.flatMap(prod.films.get).flatMap(_.tmdbId).map(o.idx -> _)).toMap
    val own: Map[Int, (Set[String], Set[String])] =
      lfPairs.iterator.filter(p => proposal.get(p.obs).contains(p.tmdbId)).map(p => p.obs -> ownAgreement(p.measures)).toMap
    val byFilm = obs.filter(o => own.contains(o.idx)).groupBy(o => proposal(o.idx))
    val evidence: Map[Int, Evidence] = own.map { case (i, (agree, deny)) =>
      val o = obs(i - startIdx)
      // Other venues listing the SAME title that back the film by their own year or director, and
      // deny it on nothing. Same title only: a production film that already merged two remakes
      // ("Candyman (1992)" under the 2021 film) would otherwise vouch for its own mistake.
      val titleKey = IdentityMeasures.key(o.listing.title)
      val others = byFilm(proposal(i)).filter(x => x.venue != o.venue && IdentityMeasures.key(x.listing.title) == titleKey &&
          own(x.idx)._1.exists(c => c == "year" || c == "director") && own(x.idx)._2.isEmpty)
        .map(_.venue).distinct.size
      i -> Evidence(proposal(i), if (others >= 2) agree + "venues" else agree, deny)
    }

    // Listing-listing pairs: listings sharing a block key (title, a banner segment, an original
    // title); a large block is sampled deterministically (each member with its next 20 by hash).
    val blocks = mutable.HashMap.empty[String, mutable.ArrayBuffer[Int]]
    obs.foreach { o =>
      (IdentityMeasures.titleShapes(o.listing) ++ o.listing.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).distinct
        .foreach(k => blocks.getOrElseUpdate(k, mutable.ArrayBuffer.empty) += o.idx)
    }
    val seenPairs = mutable.HashSet.empty[Long]
    val ll = Seq.newBuilder[LlPair]
    blocks.valuesIterator.foreach { members =>
      val ms = members.distinct.sortBy(i => MurmurHash3.stringHash(obs(i - startIdx).listingKey)).toIndexedSeq
      val n = ms.size
      // Every pair of a small block; in a large one each member with its next 20 by hash.
      val pairs: Iterator[(Int, Int)] =
        if (n <= 41) for (x <- (0 until n).iterator; y <- (x + 1 until n).iterator) yield (ms(x), ms(y))
        else for (x <- (0 until n).iterator; d <- (1 to 20).iterator) yield (ms(x), ms((x + d) % n))
      pairs.foreach { case (a, b) =>
        val (lo, hi) = if (a < b) (a, b) else (b, a)
        if (lo != hi && seenPairs.add(lo.toLong << 32 | hi.toLong)) {
          val oa = obs(lo - startIdx); val ob = obs(hi - startIdx)
          ll += LlPair(lo, hi, IdentityMeasures.listingListing(oa.listing, ob.listing, oa.venue == ob.venue, sharedChainId(oa, ob)))
        }
      }
    }
    val stats = Map(
      "listings" -> obs.size, "proposed" -> proposal.size, "proposalWithDetails" -> own.size,
      "lfPairs" -> lfPairs.size, "llPairs" -> seenPairs.size)
    println(s"[$cc] $stats")
    CountryData(cc, obs, evidence, lfPairs, ll.result(), prod.films, answers.details, stats)
  }

  // ── statistics ───────────────────────────────────────────────────────────────────────

  /** Additive (Jeffreys) smoothing for every likelihood ratio. */
  private val Alpha = 0.5

  /** One-sided Wilson upper bound of a rate at the normal quantile `z`. */
  def upperBound(x: Int, n: Int, z: Double): Double =
    if (n == 0) 1.0 else {
      val p = x.toDouble / n; val z2 = z * z
      math.min(1.0, (p + z2 / (2 * n) + z * math.sqrt(p * (1 - p) / n + z2 / (4.0 * n * n))) / (1 + z2 / n))
    }

  /** One-sided 95% Wilson upper bound of a rate. */
  def upper95(x: Int, n: Int): Double = upperBound(x, n, 1.6448536269514722)

  /** The standard normal quantile, by bisection on the complementary error function
   *  (Abramowitz & Stegun 7.1.26, |error| < 1.5e-7). */
  def normalQuantile(p: Double): Double = {
    def cdf(x: Double): Double = {
      val t = 1 / (1 + 0.3275911 * math.abs(x) / math.sqrt(2))
      val erfc = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429)))) *
        math.exp(-x * x / 2)
      if (x >= 0) 1 - erfc / 2 else erfc / 2
    }
    var lo = -10.0; var hi = 10.0
    (0 until 100).foreach { _ => val mid = (lo + hi) / 2; if (cdf(mid) < p) lo = mid else hi = mid }
    (lo + hi) / 2
  }

  /** G statistic of "these two cells have the same positive:negative ratio". */
  private def gStat(p1: Double, n1: Double, p2: Double, n2: Double): Double = {
    val tot = p1 + n1 + p2 + n2
    if (tot == 0) 0.0 else {
      val rowP = p1 + p2; val rowN = n1 + n2; val c1 = p1 + n1; val c2 = p2 + n2
      def term(o: Double, e: Double) = if (o <= 0 || e <= 0) 0.0 else o * math.log(o / e)
      2 * (term(p1, rowP * c1 / tot) + term(n1, rowN * c1 / tot) + term(p2, rowP * c2 / tot) + term(n2, rowN * c2 / tot))
    }
  }

  /** The 5% critical value of chi-square with one degree of freedom: two adjacent bins merge
   *  while their ratios are not significantly different — a statistical convention, not a
   *  domain constant. */
  private val MergeCritical = 3.841458820694124

  /** Adjacent integer values merged into bins until every neighbour pair differs significantly. */
  def mergeBins(counts: Seq[(Double, (Int, Int))]): Seq[(Double, Double, Int, Int)] = {
    val bins = mutable.ArrayBuffer.from(counts.sortBy(_._1).map { case (v, (p, n)) => (v, v, p, n) })
    var merging = bins.size > 1
    while (merging) {
      val gs = (0 until bins.size - 1).map { i =>
        val a = bins(i); val b = bins(i + 1)
        // Scale by the class totals so the test compares likelihood RATIOS, not raw counts.
        i -> gStat(a._3.toDouble, a._4.toDouble, b._3.toDouble, b._4.toDouble)
      }
      val (i, g) = gs.minBy(_._2)
      if (g < MergeCritical) {
        val a = bins(i); val b = bins(i + 1)
        bins(i) = (a._1, b._2, a._3 + b._3, a._4 + b._4); bins.remove(i + 1)
        merging = bins.size > 1
      } else merging = false
    }
    bins.toSeq
  }

  def llr(pos: Int, neg: Int, totPos: Int, totNeg: Int, cells: Int): Double =
    math.log((pos + Alpha) / (totPos + Alpha * cells)) - math.log((neg + Alpha) / (totNeg + Alpha * cells))

  /** Pool-adjacent-violators: a non-decreasing fit of labels by score, as (score, probability)
   *  knots at each block's mean score. */
  def isotonic(rows: Seq[(Double, Boolean)]): (Seq[Double], Seq[Double]) = {
    val sorted = rows.sortBy(_._1)
    val xs = mutable.ArrayBuffer.empty[Double]; val ys = mutable.ArrayBuffer.empty[Double]; val ws = mutable.ArrayBuffer.empty[Double]
    sorted.foreach { case (x, y) =>
      xs += x; ys += (if (y) 1.0 else 0.0); ws += 1.0
      while (ys.size > 1 && ys(ys.size - 2) >= ys.last) {
        val w = ws(ws.size - 2) + ws.last
        val y2 = (ys(ys.size - 2) * ws(ws.size - 2) + ys.last * ws.last) / w
        val x2 = (xs(xs.size - 2) * ws(ws.size - 2) + xs.last * ws.last) / w
        xs.remove(xs.size - 1); ys.remove(ys.size - 1); ws.remove(ws.size - 1)
        xs(xs.size - 1) = x2; ys(ys.size - 1) = y2; ws(ws.size - 1) = w
      }
    }
    // Keep probabilities off exact 0/1 by the same Jeffreys smoothing, per block.
    // Smoothing a small block can dip it under a larger one before it: re-impose the order.
    val smoothed = ys.indices.map(i => (ys(i) * ws(i) + Alpha) / (ws(i) + 2 * Alpha))
    (xs.toSeq, smoothed.scanLeft(0.0)(math.max).tail)
  }

  def auc(rows: Seq[(Double, Boolean)]): Double = {
    val sorted = rows.sortBy(_._1).toIndexedSeq
    var rankSum = 0.0; var i = 0
    while (i < sorted.size) {
      var j = i; while (j < sorted.size && sorted(j)._1 == sorted(i)._1) j += 1
      val avg = (i + j + 1) / 2.0
      (i until j).foreach(k => if (sorted(k)._2) rankSum += avg)
      i = j
    }
    val np = sorted.count(_._2).toDouble; val nn = sorted.size - np
    if (np == 0 || nn == 0) Double.NaN else (rankSum - np * (np + 1) / 2) / (np * nn)
  }

  // ── fitting one scope ────────────────────────────────────────────────────────────────

  /** Missing sides that are an artefact of the RECORDING, not of the evidence, weighted 0. */
  val Neutral: Map[(String, String), String] = Map(
    ("director", "film")      -> "the candidate's credits were not fetched in the recorded trees (only candidates the pipeline examined have them), so missingness marks the sampling, not the film",
    ("runtime.delta", "film") -> "the candidate's details were not fetched in the recorded trees",
    ("country", "film")       -> "the candidate's details were not fetched in the recorded trees",
    ("search.rank", "not-returned") -> "negatives are drawn from the title search itself, so only a positive can be absent from it: the value marks the sampling")

  /** One labelled pair. `unit` is what makes two pairs independent evidence: the family and the
   *  candidate. A wide release lists one film thousands of times with the same evidence, and those
   *  pairs are one observation, not thousands: counts, bin tests and false-veto bounds all count
   *  distinct units. */
  final case class Row(split: String, family: String, unit: String, country: String, measures: Map[String, Measure],
                       label: Set[String] => Option[Boolean])

  final case class Table(signal: String, weights: SignalWeights, positives: Int, negatives: Int)

  def fitSignal(signal: String, rows: Seq[Row], labelFor: String => Set[String]): Table = {
    val lab = rows.flatMap(r => r.label(labelFor(signal)).map(y => (r.unit, r.measures.get(signal), y))).distinct.map(t => t._2 -> t._3)
    val totP = lab.count(_._2); val totN = lab.size - totP
    val numeric = lab.exists(_._1.exists(_.isInstanceOf[Number]))
    val missing = lab.collect { case (Some(Missing(s)), y) => s -> y }.groupMapReduce(_._1)(p => if (p._2) (1, 0) else (0, 1))((a, b) => (a._1 + b._1, a._2 + b._2))
    if (numeric) {
      val values = lab.collect { case (Some(Number(x)), y) => x -> y }
        .groupMapReduce(_._1)(p => if (p._2) (1, 0) else (0, 1))((a, b) => (a._1 + b._1, a._2 + b._2)).toSeq
      val bins = mergeBins(values)
      val cells = bins.size + missing.size
      // Contiguous: an unobserved value between two bins belongs to the nearer one.
      val cuts = bins.sliding(2).collect { case Seq(a, b) => (a._2 + b._1) / 2 }.toIndexedSeq
      val bs = bins.zipWithIndex.map { case ((_, _, p, n), i) =>
        Bin(if (i == 0) None else Some(cuts(i - 1)), if (i == bins.size - 1) None else Some(cuts(i)), llr(p, n, totP, totN, cells), p, n)
      }
      val miss = missing.map { case (s, (p, n)) => s -> (if (Neutral.contains(signal -> s)) 0.0 else llr(p, n, totP, totN, cells)) }
      Table(signal, SignalWeights("numeric", bins = bs, missing = miss,
        counts = missing.map { case (s, (p, n)) => s"missing:$s" -> Seq(p, n) },
        neutral = missing.keys.flatMap(s => Neutral.get(signal -> s).map(s -> _)).toMap), totP, totN)
    } else {
      val cats = lab.collect { case (Some(Category(v)), y) => v -> y }.groupMapReduce(_._1)(p => if (p._2) (1, 0) else (0, 1))((a, b) => (a._1 + b._1, a._2 + b._2))
      val cells = cats.size + missing.size
      val miss = missing.map { case (s, (p, n)) => s -> (if (Neutral.contains(signal -> s)) 0.0 else llr(p, n, totP, totN, cells)) }
      Table(signal, SignalWeights("categorical",
        categories = cats.map { case (v, (p, n)) => v -> llr(p, n, totP, totN, cells) },
        missing = miss,
        counts = cats.map { case (v, (p, n)) => v -> Seq(p, n) } ++ missing.map { case (s, (p, n)) => s"missing:$s" -> Seq(p, n) },
        neutral = missing.keys.flatMap(s => Neutral.get(signal -> s).map(s -> _)).toMap), totP, totN)
    }
  }

  final case class Fitted(scope: String, tables: Seq[Table], prior: Double, calibration: Calibration) {
    def model: ScopeModel = ScopeModel(prior, tables.map(t => t.signal -> t.weights).toMap, calibration)
    def logOdds(m: Map[String, Measure]): Double = prior + tables.map(t => t.weights.weight(m.get(t.signal))).sum
    def probability(m: Map[String, Measure]): Double = calibration(logOdds(m))
  }

  /** A split's labelled pairs as independent units: each distinct (unit, score, label) once. */
  def units(rows: Seq[Row], split: String, score: Map[String, Measure] => Double): Seq[(Double, Boolean)] =
    rows.iterator.filter(_.split == split).flatMap(r => r.label(Set.empty).map(y => (r.unit, score(r.measures), y)))
      .toSeq.distinct.map(u => u._2 -> u._3)

  def fit(scope: String, signals: Seq[String], rows: Seq[Row]): Fitted = {
    val train = rows.filter(_.split == "train")
    def excluded(signal: String): Set[String] = corroboratorOf(signal).toSet
    val tables = signals.map(s => fitSignal(s, train, excluded))
    val full = train.flatMap(_.label(Set.empty))
    val prior = math.log((full.count(identity) + Alpha) / (full.count(!_) + Alpha))
    val raw = Fitted(scope, tables, prior, Calibration("isotonic", Nil, Nil))
    val cal = units(rows, "calibration", raw.logOdds).map(u => u._1 -> u._2)
    val (xs, ps) = isotonic(cal)
    raw.copy(calibration = Calibration("isotonic", xs, ps))
  }

  // ── cannot-link rules ────────────────────────────────────────────────────────────────

  /** One atom of a rule: a category set, or a number at least a threshold. */
  final case class Atom(signal: String, in: Seq[String] = Nil, atLeast: Option[Double] = None) {
    def holds(m: Map[String, Measure]): Boolean = m.get(signal) match {
      case Some(Category(v)) => in.contains(v)
      case Some(Number(x))   => in.isEmpty && atLeast.exists(x >= _)
      case _                 => false
    }
    def condition: Condition = Condition(signal, in, atLeast, None)
    def render: String = if (in.nonEmpty) s"$signal in {${in.mkString(",")}}" else f"$signal >= ${atLeast.get}%.0f"
  }

  final case class RuleStats(fp: Int, pos: Int, tn: Int, neg: Int, z: Double = 1.6448536269514722) {
    def falseVeto: Double = if (pos == 0) 0.0 else fp.toDouble / pos
    def upper: Double     = upperBound(fp, pos, z)
    /** Does the rule fire on different films significantly more than on same films? */
    def significant: Boolean = gStat(tn.toDouble, (neg - tn).toDouble, fp.toDouble, (pos - fp).toDouble) >= z * z &&
      tn.toDouble / math.max(1, neg) > fp.toDouble / math.max(1, pos)
    def trueVeto: Double  = if (neg == 0) 0.0 else tn.toDouble / neg
  }

  private val labelledCache = mutable.HashMap.empty[(Int, Set[String]), IndexedSeq[(String, Map[String, Measure], Boolean)]]

  /** The rows labelled without `excluded`'s corroborators, computed once per row set. */
  private def labelled(rows: Seq[Row], excluded: Set[String]): IndexedSeq[(String, Map[String, Measure], Boolean)] =
    labelledCache.getOrElseUpdate((System.identityHashCode(rows), excluded),
      rows.iterator.flatMap(r => r.label(excluded).map(y => (r.unit, r.measures, y))).toIndexedSeq)

  /** A rule's rates over independent UNITS: a unit counts as vetoed when the rule fires on any of
   *  its pairs — conservative for the false-veto bound. */
  def unitStats(fires: Map[String, Measure] => Boolean, rows: IndexedSeq[(String, Map[String, Measure], Boolean)]): RuleStats = {
    val pos = mutable.HashSet.empty[String]; val neg = mutable.HashSet.empty[String]
    val fp = mutable.HashSet.empty[String]; val tn = mutable.HashSet.empty[String]
    rows.foreach { case (u, m, y) =>
      if (y) { pos += u; if (fires(m)) fp += u } else { neg += u; if (fires(m)) tn += u }
    }
    RuleStats(fp.size, pos.size, tn.size, neg.size)
  }

  def ruleStats(atoms: Seq[Atom], rows: Seq[Row]): RuleStats =
    unitStats(m => atoms.forall(_.holds(m)), labelled(rows, atoms.flatMap(a => corroboratorOf(a.signal)).toSet))

  /** Search conjunctions of one or two atoms for the rules whose false-veto upper bound stays
   *  under `epsilon`, each at the threshold that vetoes the most different-film pairs. */
  def deriveRules(scope: String, rows: Seq[Row], categorical: Seq[Atom], numeric: Seq[String], epsilon: Option[Double]): Seq[(Seq[Atom], RuleStats)] = {
    def thresholds(signal: String): Seq[Double] =
      rows.iterator.flatMap(r => number(r.measures.get(signal))).filter(x => x > 0 && x <= 120).toSeq.distinct.sorted
    val numAtoms: Map[String, Seq[Double]] = numeric.map(s => s -> thresholds(s)).toMap
    val shapes: Seq[(Seq[Atom], Option[String], Option[String])] =
      categorical.map(a => (Seq(a), None, None)) ++
        numeric.map(s => (Nil, Some(s), None)) ++
        (for (a <- categorical; b <- categorical if a.signal < b.signal) yield (Seq(a, b), None, None)) ++
        (for (a <- categorical; s <- numeric if corroboratorOf(a.signal).isEmpty || corroboratorOf(a.signal) != corroboratorOf(s)) yield (Seq(a), Some(s), None)) ++
        (for (s <- numeric; t <- numeric if s < t && corroboratorOf(s) != corroboratorOf(t)) yield (Nil, Some(s), Some(t)))
    // Every shape is a hypothesis: bound each at a Bonferroni-corrected confidence, so the best of
    // many searched shapes does not pass on luck (the winner's curse).
    val z = normalQuantile(1 - 0.05 / shapes.size)
    println(f"[$scope] ${shapes.size} rule shapes, bound at z = $z%.3f")
    // Certified (default): the rule fired on no same-film unit. With an explicit epsilon: its
    // Bonferroni-corrected upper bound is under it.
    def passes(st: RuleStats): Boolean = epsilon.fold(st.fp == 0)(st.upper <= _)
    def best(fixed: Seq[Atom], free: Option[String]): Option[(Seq[Atom], RuleStats)] = free match {
      case None =>
        val st = ruleStats(fixed, rows).copy(z = z); Option.when(passes(st) && st.significant)(fixed -> st)
      case Some(sig) =>
        // The false-veto rate falls as the threshold rises: find the lowest passing threshold.
        val ts = numAtoms(sig).toIndexedSeq
        var lo = 0; var hi = ts.size - 1; var found: Option[(Seq[Atom], RuleStats)] = None
        while (lo <= hi) {
          val mid = (lo + hi) / 2
          val atoms = fixed :+ Atom(sig, atLeast = Some(ts(mid)))
          val st = ruleStats(atoms, rows).copy(z = z)
          if (passes(st)) { if (st.significant) found = Some(atoms -> st); hi = mid - 1 } else lo = mid + 1
        }
        found
    }
    shapes.flatMap {
      case (fixed, None, _)          => best(fixed, None)
      case (fixed, Some(s), None)    => best(fixed, Some(s))
      case (fixed, Some(s), Some(t)) =>
        // Two free numbers: scan the first, binary-search the second.
        numAtoms(s).flatMap(v => best(fixed :+ Atom(s, atLeast = Some(v)), Some(t))).maxByOption(_._2.tn)
    }.map { case (atoms, st) => atoms.sortBy(_.signal) -> st }.distinctBy(_._1)
  }

  /** Keep rules greedily by how many different-film pairs they veto that no kept rule already
   *  does; a rule that adds none is redundant and dropped. */
  def selectRules(candidates: Seq[(Seq[Atom], RuleStats)], rows: Seq[Row]): Seq[(Seq[Atom], RuleStats)] = {
    val labelled = rows.flatMap(r => r.label(Set.empty).map(r -> _)).toIndexedSeq
    val negIdx = labelled.indices.filter(i => !labelled(i)._2)
    val posIdx = labelled.indices.filter(i => labelled(i)._2)
    val posUnits = posIdx.map(i => labelled(i)._1.unit).toSet
    val vetoedNeg = mutable.BitSet.empty; val vetoedPos = mutable.HashSet.empty[String]
    val kept = mutable.ArrayBuffer.empty[(Seq[Atom], RuleStats)]
    var remaining = candidates.sortBy(c => (-c._2.tn, c._2.fp, c._1.map(_.render).mkString))
    var going = true
    while (going && remaining.nonEmpty) {
      val scored = remaining.map { c =>
        val gain = negIdx.count(i => !vetoedNeg(i) && c._1.forall(_.holds(labelled(i)._1.measures)))
        c -> gain
      }.filter(_._2 > 0).sortBy { case (c, g) => (-g, c._2.fp, c._1.map(_.render).mkString) }
      scored.headOption match {
        case None => going = false
        case Some((c, _)) =>
          val newPos = posIdx.filter(i => c._1.forall(_.holds(labelled(i)._1.measures))).map(i => labelled(i)._1.unit).toSet -- vetoedPos
          if (posUnits.nonEmpty) {
            kept += c
            negIdx.foreach(i => if (c._1.forall(_.holds(labelled(i)._1.measures))) vetoedNeg += i)
            newPos.foreach(vetoedPos += _)
          }
          remaining = remaining.filterNot(_._1 == c._1)
      }
    }
    kept.toSeq
  }

  // ── today's vetoes, re-expressed as signal predicates ───────────────────────────────

  private def cat(m: Map[String, Measure], s: String) = category(m.get(s))
  private def num(m: Map[String, Measure], s: String) = number(m.get(s))

  /** `MixedFilmDetector.deniesFilm` (the DeniedCandidate / VenueDeniesFilm veto). */
  def venueDeniesFilm(m: Map[String, Measure]): Boolean =
    num(m, "year.distance").exists(_ > 5) && cat(m, "director").contains("different")

  /** `MixedFilmDetector.listingDeniesFilm` (the DecorationVeto / ListingDeniesFilm veto). */
  def listingDeniesFilm(m: Map[String, Measure]): Boolean =
    cat(m, "director").exists(Set("different", "shared_name", "incomparable")) &&
      (num(m, "runtime.delta").exists(_ > 2) || num(m, "year.distance").exists(_ > 5))

  /** `MixedFilmDetector.wouldAddASecondFilm` (OriginalTitleNamesAnotherFilm). */
  def originalTitleNamesAnotherFilm(m: Map[String, Measure]): Boolean =
    cat(m, "originalTitle").contains("disjoint") && !cat(m, "director").contains("same_person") &&
      (num(m, "runtime.delta") match {
        case Some(d) => d > 2
        case None    => num(m, "year.distance").orElse(num(m, "year.delta").map(math.abs)).exists(_ > 1)
      })

  /** The containment fold's venue denial (the Faust refusal): a decorated-title match denied. */
  def faustFoldRefusal(m: Map[String, Measure]): Boolean =
    cat(m, "title").exists(Set("segment", "contains")) && venueDeniesFilm(m)

  /** `MixedFilmDetector.describeDifferentFilms` between two listings (CinemasDescribeDifferentFilms). */
  def cinemasDescribeDifferentFilms(m: Map[String, Measure]): Boolean =
    cat(m, "originalTitle").contains("disjoint") && !cat(m, "director").contains("same_person") &&
      (num(m, "runtime.delta") match {
        case Some(d) => d > 2
        case None    => num(m, "year.delta").exists(_ > 1)
      })

  /** `ListingConstraints.venueCreditsApart`. */
  def venueCreditsApart(m: Map[String, Measure]): Boolean =
    cat(m, "venue").contains("same") && cat(m, "director").exists(Set("different", "shared_name", "incomparable"))

  val ExistingLf: Seq[(String, Map[String, Measure] => Boolean, Set[String])] = Seq(
    ("VenueDeniesFilm (DeniedCandidate)", venueDeniesFilm, Set("year", "director")),
    ("ListingDeniesFilm (DecorationVeto)", listingDeniesFilm, Set("year", "director", "runtime")),
    ("OriginalTitleNamesAnotherFilm", originalTitleNamesAnotherFilm, Set("year", "director", "originalTitle", "runtime")),
    ("Faust fold refusal (containment + venue denial)", faustFoldRefusal, Set("year", "director")))
  val ExistingLl: Seq[(String, Map[String, Measure] => Boolean, Set[String])] = Seq(
    ("CinemasDescribeDifferentFilms", cinemasDescribeDifferentFilms, Set("year", "director", "originalTitle", "runtime")),
    ("VenueCreditsApart", venueCreditsApart, Set("director")))

  // ── the run ──────────────────────────────────────────────────────────────────────────

  final case class Scope(name: String, signals: Seq[String], rows: Seq[Row])

  val LfSignals: Seq[String] = Seq("title", "originalTitle", "year.delta", "director", "runtime.delta", "country",
    "search.rank", "popularity.log2", "rivals", "venues.corroborating")
  val LlSignals: Seq[String] = Seq("title", "originalTitle", "year.delta", "director", "runtime.delta", "venue", "chainId")

  private def splitOf(family: String): String = {
    val h = java.lang.Math.floorMod(MurmurHash3.stringHash(family), 10)
    if (h < 5) "train" else if (h < 7) "calibration" else "test"
  }

  def run(cfg: Config): Unit = {
    Files.createDirectories(cfg.reportDir)
    val countries = Country.all.filter(c => cfg.countries.contains(c.code))
    var next = 0
    val data = countries.map { c => val d = load(cfg, c, next); next += d.obs.size; d }
    val obsAll: Map[Int, Obs] = data.flatMap(_.obs).map(o => o.idx -> o).toMap
    val evidence0: Map[Int, Evidence] = data.flatMap(_.evidence).toMap

    // Families: listings joined by title key or by production's film; split per family.
    val parent = mutable.HashMap.empty[String, String]
    def find(x: String): String = { val p = parent.getOrElse(x, x); if (p == x) x else { val r = find(p); parent(x) = r; r } }
    def union(a: String, b: String): Unit = { val (ra, rb) = (find(a), find(b)); if (ra != rb) { if (ra < rb) parent(rb) = ra else parent(ra) = rb } }
    obsAll.values.foreach { o =>
      val me = s"l:${o.idx}"
      union(me, s"t:${o.country}:${IdentityMeasures.key(o.listing.title)}")
      o.prodFilm.foreach(f => union(me, s"f:${o.country}:$f"))
    }
    val familyRep = obsAll.values.groupBy(o => find(s"l:${o.idx}")).map { case (root, os) => root -> os.map(_.listingKey).min }
    val familyOf: Map[Int, String] = obsAll.values.map(o => o.idx -> familyRep(find(s"l:${o.idx}"))).toMap
    val split: Map[Int, String] = familyOf.view.mapValues(splitOf).toMap

    // The runtime DENIES a proposal where its own table (fitted on labels that never read the
    // runtime) says a different film is at least 19 times likelier than the same one — the 95%
    // convention as a likelihood ratio. Without it, two weak agreements (a year within one, other
    // venues listing the same spelling) label "Lalka (ale to horror)" (2025, 82 min) as the
    // 162-minute "Lalka" (2026) production merged it into.
    def lfRowsOf(ev: Map[Int, Evidence], k: Int): Seq[Row] = data.flatMap(_.lf).map { p =>
      val e = ev.get(p.obs)
      Row(split(p.obs), familyOf(p.obs), s"${familyOf(p.obs)}|${p.tmdbId}", obsAll(p.obs).country, p.measures,
        excluded => e.filter(_.positive(excluded, k)).map(_.tmdbId == p.tmdbId))
    }
    val runtimeDeny = fitSignal("runtime.delta", lfRowsOf(evidence0, 2).filter(_.split == "train"), _ => Set("runtime")).weights
    val RuntimeDenial = -math.log(19)
    val proposalRuntime: Map[Int, Option[Measure]] = data.flatMap(_.lf).iterator
      .filter(p => evidence0.get(p.obs).exists(_.tmdbId == p.tmdbId)).map(p => p.obs -> p.measures.get("runtime.delta")).toMap
    val evidence: Map[Int, Evidence] = evidence0.map { case (i, e) =>
      val m = proposalRuntime.getOrElse(i, None)
      i -> (if (m.exists(_.isInstanceOf[Number]) && runtimeDeny.weight(m) <= RuntimeDenial) e.copy(deny = e.deny + "runtime") else e)
    }

    def lfRows(k: Int): Seq[Row] = data.flatMap(_.lf).map { p =>
      val ev = evidence.get(p.obs)
      Row(split(p.obs), familyOf(p.obs), s"${familyOf(p.obs)}|${p.tmdbId}", obsAll(p.obs).country, p.measures,
        excluded => ev.filter(_.positive(excluded, k)).map(_.tmdbId == p.tmdbId))
    }
    def llRows(k: Int): Seq[Row] = data.flatMap(_.ll).map { p =>
      val (ea, eb) = (evidence.get(p.a), evidence.get(p.b))
      Row(split(p.a), familyOf(p.a), familyOf(p.a) + "|" + familyOf(p.b), obsAll(p.a).country, p.measures, excluded =>
        for (a <- ea if a.positive(excluded, k); b <- eb if b.positive(excluded, k)) yield a.tmdbId == b.tmdbId)
    }
    // Rule labels: one corroborator left is enough (weaker labels can only OVERSTATE a rule's
    // false vetoes, so the bound errs toward keeping a veto out).
    def ruleRows(rows: Int => Seq[Row]): Seq[Row] = rows(1)

    val lf2 = lfRows(2); val ll2 = llRows(2)
    val lfFit = fit(IdentityMeasures.ListingFilm, LfSignals, lf2)
    val llFit = fit(IdentityMeasures.ListingListing, LlSignals, ll2)
    val lfFit3 = fit(IdentityMeasures.ListingFilm, LfSignals, lfRows(3))
    val llFit3 = fit(IdentityMeasures.ListingListing, LlSignals, llRows(3))

    val report = new StringBuilder
    def line(s: String = ""): Unit = { report.append(s).append('\n'); () }

    // ── counts ──
    val totalListings = obsAll.size
    val proposed = data.map(_.stats("proposed")).sum
    val withDetails = evidence.size
    val corroborated2 = evidence.values.count(_.positive(Set.empty, 2))
    val corroborated3 = evidence.values.count(_.positive(Set.empty, 3))
    val contradicted = evidence.filter(_._2.contradicted)
    // Per independent unit (family and film): a wide release's thousand listings are one filing.
    val contradictedUnits = contradicted.keys.map(i => familyOf(i) -> evidence(i).tmdbId).toSet
    val corroboratedUnits = evidence.filter(_._2.positive(Set.empty, 2)).keys.map(i => familyOf(i) -> evidence(i).tmdbId).toSet
    val todayWrong = contradictedUnits.size.toDouble / (contradictedUnits.size + corroboratedUnits.size)
    line("### Label counts")
    line()
    line("| country | listings | filed on a production tmdbId | with TMDB details | corroborated (k=2) | corroborated (k=3) | contradicted | unlabelled |")
    line("|---|---|---|---|---|---|---|---|")
    data.foreach { d =>
      val ev = d.evidence.values
      val c2 = ev.count(_.positive(Set.empty, 2)); val cx = ev.count(_.contradicted)
      line(s"| ${d.code} | ${d.obs.size} | ${d.stats("proposed")} | ${d.stats("proposalWithDetails")} | $c2 | ${ev.count(_.positive(Set.empty, 3))} | $cx | ${d.obs.size - c2 - cx} |")
    }
    line(s"| all | $totalListings | $proposed | $withDetails | $corroborated2 | $corroborated3 | ${contradicted.size} | ${totalListings - corroborated2 - contradicted.size} |")
    line()
    line(f"Today's measured wrong rate: ${contradictedUnits.size} contradicted of ${contradictedUnits.size + corroboratedUnits.size} decisive production filings, counted per family and film = ${100 * todayWrong}%.2f%% (per listing: ${contradicted.size} of ${contradicted.size + corroborated2}).")
    line()

    // ── signal tables ──
    def tables(f: Fitted, title: String): Unit = {
      line(s"### $title")
      line()
      f.tables.foreach { t =>
        line(s"**${t.signal}** (fitted on ${t.positives} same / ${t.negatives} different pairs, labels without `${corroboratorOf(t.signal).getOrElse("-")}`)")
        line()
        line("| value | same | different | log-LR |")
        line("|---|---|---|---|")
        t.weights.bins.foreach { b =>
          val range = (b.atLeast, b.atMost) match {
            case (None, None)         => "any"
            case (None, Some(h))      => f"≤ $h%.0f"
            case (Some(l), None)      => f"≥ $l%.0f"
            case (Some(l), Some(h))   => if (l == h) f"$l%.0f" else f"$l%.0f … $h%.0f"
          }
          line(f"| $range | ${b.positives} | ${b.negatives} | ${b.weight}%+.2f |")
        }
        t.weights.categories.toSeq.sortBy(-_._2).foreach { case (v, w) =>
          val c = t.weights.counts.getOrElse(v, Seq(0, 0)); line(f"| $v | ${c.head} | ${c(1)} | $w%+.2f |")
        }
        t.weights.missing.toSeq.sortBy(_._1).foreach { case (s, w) =>
          val c = t.weights.counts.getOrElse(s"missing:$s", Seq(0, 0))
          line(f"| missing:$s | ${c.head} | ${c(1)} | $w%+.2f${if (t.weights.neutral.contains(s)) " (neutral)" else ""} |")
        }
        line()
      }
    }
    tables(lfFit, "Listing ↔ film signal tables")
    tables(llFit, "Listing ↔ listing signal tables")

    // ── calibration quality on held-out ──
    def heldOut(f: Fitted, rows: Seq[Row]): Seq[(Double, Double, Boolean)] =
      units(rows, "test", f.logOdds).map { case (x, y) => (x, f.calibration(x), y) }
    def quality(name: String, f: Fitted, rows: Seq[Row]): JsObject = {
      val h = heldOut(f, rows)
      val brier = h.map { case (_, p, y) => val t = if (y) 1.0 else 0.0; (p - t) * (p - t) }.sum / h.size
      val logLoss = -h.map { case (_, p, y) => math.log(math.max(1e-12, if (y) p else 1 - p)) }.sum / h.size
      val a = auc(h.map(x => x._1 -> x._3))
      val bins = (0 until 10).map { b =>
        val in = h.filter { case (_, p, _) => math.min(9, (p * 10).toInt) == b }
        (b, in.size, if (in.isEmpty) Double.NaN else in.map(_._2).sum / in.size, if (in.isEmpty) Double.NaN else in.count(_._3).toDouble / in.size)
      }
      line(s"### Calibration, $name (held-out split: ${h.size} units, ${h.count(_._3)} same)")
      line()
      line(f"AUC ${a}%.4f, Brier ${brier}%.4f, log-loss ${logLoss}%.4f.")
      line()
      line("| predicted bin | units | mean predicted | observed same |")
      line("|---|---|---|---|")
      bins.filter(_._2 > 0).foreach { case (b, n, mp, obsd) => line(f"| ${b / 10.0}%.1f–${(b + 1) / 10.0}%.1f | $n | $mp%.3f | $obsd%.3f |") }
      line()
      line("| threshold | precision | recall | wrong among accepted |")
      line("|---|---|---|---|")
      Seq(0.5, 0.8, 0.9, 0.95, 0.98, 0.99).foreach { t =>
        val acc = h.filter(_._2 >= t); val tp = acc.count(_._3); val allPos = h.count(_._3)
        line(f"| $t%.2f | ${if (acc.isEmpty) Double.NaN else tp.toDouble / acc.size}%.4f | ${tp.toDouble / allPos}%.4f | ${acc.size - tp} of ${acc.size} |")
      }
      line()
      def js(x: Double) = if (x.isNaN) play.api.libs.json.JsNull else play.api.libs.json.JsNumber(BigDecimal(x))
      Json.obj("auc" -> js(a), "brier" -> js(brier), "logLoss" -> js(logLoss), "reliability" -> JsArray(bins.filter(_._2 > 0).map {
        case (b, n, mp, o) => Json.obj("bin" -> b, "pairs" -> n, "meanPredicted" -> js(mp), "observed" -> js(o)) }))
    }
    val lfQuality = quality("listing ↔ film", lfFit, lf2)
    val llQuality = quality("listing ↔ listing", llFit, ll2)

    // ── show-ratings threshold ──
    // A pair is "shown" when its probability clears the cut; it is WRONG when the pair is not one
    // film. The evaluated units are every labelled pair plus production's CONTRADICTED filings,
    // which are exactly the wrong ratings served today. The cut is the lowest whose wrong share
    // on the calibration split stays within today's measured wrong rate; the held-out split
    // reports what it does.
    val contradictedPairs: Seq[Row] = data.flatMap(_.lf).filter(p => evidence.get(p.obs).exists(e => e.contradicted && e.tmdbId == p.tmdbId))
      .map(p => Row(split(p.obs), familyOf(p.obs), s"${familyOf(p.obs)}|${p.tmdbId}", obsAll(p.obs).country, p.measures, _ => Some(false)))
    val shownRows = lf2 ++ contradictedPairs
    def shown(which: String): Seq[(Double, Boolean)] = units(shownRows, which, lfFit.probability)
    val calShown = shown("calibration"); val testShown = shown("test")
    def wrongAt(ds: Seq[(Double, Boolean)], t: Double) = { val acc = ds.filter(_._1 >= t); (acc.count(!_._2), acc.size) }
    // The lowest cut whose wrong share's one-sided 95% upper bound (not its point estimate, which
    // the held-out split does not reproduce) stays within today's rate.
    val showT = calShown.map(_._1).distinct.sorted.find { t => val (w, n) = wrongAt(calShown, t); n > 0 && upper95(w, n) <= todayWrong }.getOrElse(1.0)
    val (tw, tn) = wrongAt(testShown, showT)
    val testSame = testShown.count(_._2)
    val testContradicted = units(contradictedPairs, "test", lfFit.probability)
    // Per decision: the top candidate of each held-out family and film, as the resolver picks it.
    val testDecisions: Seq[(Double, Boolean)] = data.flatMap(_.lf).filter(p => split(p.obs) == "test").groupBy(_.obs).toSeq.flatMap { case (i, ps) =>
      evidence.get(i).filter(e => e.positive(Set.empty, 2) || e.contradicted).flatMap { ev =>
        val top = ps.maxBy(p => (lfFit.probability(p.measures), -p.tmdbId))
        if (ev.contradicted && top.tmdbId != ev.tmdbId) None // the right film is unknown: not scored
        else Some((familyOf(i), ev.tmdbId, top.tmdbId, lfFit.probability(top.measures), !ev.contradicted && top.tmdbId == ev.tmdbId))
      }
    }.distinct.map(d => d._4 -> d._5)
    line("### Show-ratings threshold")
    line()
    line(f"Target: among pairs whose ratings would be shown, the wrong share must not exceed today's measured wrong rate, ${100 * todayWrong}%.2f%%.")
    line(f"Derived on the calibration split: p ≥ $showT%.4f. Held out: $tw wrong of $tn shown (${if (tn == 0) 0.0 else 100.0 * tw / tn}%.3f%%); recall of same-film units ${100.0 * testShown.count(x => x._2 && x._1 >= showT) / math.max(1, testSame)}%.1f%%; production's contradicted filings shown: ${testContradicted.count(_._1 >= showT)} of ${testContradicted.size}.")
    line()
    line("| p ≥ | held-out units shown | wrong | wrong rate | per-decision: shown / wrong |")
    line("|---|---|---|---|---|")
    Seq(0.5, 0.8, 0.9, 0.95, showT, 0.99).distinct.sorted.foreach { t =>
      val (w, n) = wrongAt(testShown, t); val (dw, dn) = wrongAt(testDecisions, t)
      line(f"| $t%.4f | $n | $w | ${if (n == 0) 0.0 else 100.0 * w / n}%.3f%% | $dn / $dw |")
    }
    line()

    // ── score cannot-link threshold ──
    def clThreshold(f: Fitted, rows: Seq[Row]): (Double, Map[String, Double]) = {
      val cal = units(rows, "calibration", f.probability)
      val pos = cal.filter(_._2).map(_._1).sorted.toIndexedSeq
      // Certified (the default): below every same-film unit of the calibration split. With an
      // explicit epsilon: the largest cut whose share of same-film units below it keeps its
      // one-sided 95% upper bound under epsilon.
      val cut = cfg.epsilon match {
        case None      => pos.headOption.getOrElse(0.0)
        case Some(eps) => (0 to pos.size).reverse.find(i => upper95(i, pos.size) <= eps).map(i => if (i == 0) pos.headOption.getOrElse(0.0) else pos(i - 1)).getOrElse(0.0)
      }
      val test = units(rows, "test", f.probability)
      val fp = test.count(t => t._2 && t._1 < cut); val np = test.count(_._2)
      val tn = test.count(t => !t._2 && t._1 < cut); val nn = test.count(!_._2)
      cut -> Map("heldOutFalseVeto" -> fp.toDouble / math.max(1, np), "heldOutFalseVetoUpper95" -> upper95(fp, np),
        "heldOutTrueVeto" -> tn.toDouble / math.max(1, nn), "heldOutSame" -> np.toDouble, "heldOutDifferent" -> nn.toDouble)
    }
    val (lfCl, lfClM) = clThreshold(lfFit, lf2)
    val (llCl, llClM) = clThreshold(llFit, ll2)
    line("### Cannot-link score thresholds")
    line()
    line(cfg.epsilon.fold("Certified: each cut sits below every same-film unit of the calibration split.")(e => f"Error bound ε = $e%.4f (one-sided 95%% upper bound of the false-veto rate)."))
    line(f"- listing ↔ film: p < $lfCl%.5f — held-out false veto ${100 * lfClM("heldOutFalseVeto")}%.3f%% (≤ ${100 * lfClM("heldOutFalseVetoUpper95")}%.3f%%), vetoes ${100 * lfClM("heldOutTrueVeto")}%.1f%% of different-film units.")
    line(f"- listing ↔ listing: p < $llCl%.5f — held-out false veto ${100 * llClM("heldOutFalseVeto")}%.3f%% (≤ ${100 * llClM("heldOutFalseVetoUpper95")}%.3f%%), vetoes ${100 * llClM("heldOutTrueVeto")}%.1f%% of different-film units.")
    line()

    // ── existing vetoes ──
    def existing(title: String, vetoes: Seq[(String, Map[String, Measure] => Boolean, Set[String])], rows: Int => Seq[Row]): Seq[JsObject] = {
      val rs = ruleRows(rows)
      line(s"### Today's vetoes, measured ($title)")
      line()
      line("| veto | fires on same-film (false veto) | upper 95% | fires on different-film (true veto) | per country false veto | verdict |")
      line("|---|---|---|---|---|---|")
      vetoes.map { case (name, pred, uses) =>
        def stats(sub: Seq[Row]) =
          unitStats(pred, sub.iterator.flatMap(r => r.label(uses).map(y => (r.unit, r.measures, y))).toIndexedSeq)
        val st = stats(rs)
        val perCountry = rs.groupBy(_.country).toSeq.sortBy(_._1).map { case (c, sub) => val s = stats(sub); f"$c ${s.fp}/${s.pos}" }.mkString(", ")
        val verdict =
          if (st.fp == 0) "certified: never fired on a same-film unit"
          else if (st.upper <= todayWrong) f"within today's wrong rate (${st.fp} false vetoes), an ordinary negative weight rather than a cannot-link"
          else "too strict: vetoes true matches beyond today's wrong rate"
        line(f"| $name | ${st.fp} of ${st.pos} (${100 * st.falseVeto}%.3f%%) | ${100 * st.upper}%.3f%% | ${st.tn} of ${st.neg} (${100 * st.trueVeto}%.2f%%) | $perCountry | $verdict |")
        Json.obj("veto" -> name, "falseVetoes" -> st.fp, "same" -> st.pos, "trueVetoes" -> st.tn, "different" -> st.neg,
          "falseVetoUpper95" -> st.upper, "verdict" -> verdict)
      }
    }
    val lfExisting = existing("listing ↔ film", ExistingLf, lfRows)
    line()
    val llExisting = existing("listing ↔ listing", ExistingLl, llRows)
    line()
    // The bare-listing home: a must-link, measured as how often a bare listing is the film its
    // exact-titled sibling is.
    val bare = ruleRows(llRows).filter { r =>
      cat(r.measures, "title").contains("exact") &&
        r.measures.get("year.delta").exists(_.isInstanceOf[Missing]) && r.measures.get("runtime.delta").exists(_.isInstanceOf[Missing])
    }.flatMap(_.label(Set.empty))
    line(f"Bare-listing home (must-link): of ${bare.size} labelled exact-title pairs where a side publishes neither year nor runtime, ${bare.count(identity)} are one film (${100.0 * bare.count(identity) / math.max(1, bare.size)}%.2f%%).")
    line()

    // ── derived cannot-link rules ──
    def derive(scope: String, rows: Int => Seq[Row], categorical: Seq[Atom], numeric: Seq[String],
               existingPreds: Seq[(String, Map[String, Measure] => Boolean, Set[String])]): Seq[CannotLinkRule] = {
      val fitRows = ruleRows(rows).filter(_.split != "test")
      val testRows = ruleRows(rows).filter(_.split == "test")
      val candidates = deriveRules(scope, fitRows, categorical, numeric, cfg.epsilon)
      val kept = selectRules(candidates, fitRows)
      line(s"### Derived cannot-link rules ($scope)")
      line()
      line("| rule | fit: false veto (Bonferroni bound) | fit: true veto | held out: false veto | held out: true veto | covered by today's vetoes |")
      line("|---|---|---|---|---|---|")
      kept.map { case (atoms, st) =>
        val ts = ruleStats(atoms, testRows)
        val negs = fitRows.filter(r => r.label(Set.empty).contains(false) && atoms.forall(_.holds(r.measures)))
        val covered = negs.count(r => existingPreds.exists(_._2(r.measures)))
        val name = atoms.map(_.render).mkString(" AND ")
        line(f"| $name | ${st.fp}/${st.pos} (${100 * st.upper}%.3f%%) | ${st.tn}/${st.neg} (${100 * st.trueVeto}%.2f%%) | ${ts.fp}/${ts.pos} (${100 * ts.falseVeto}%.3f%%) | ${ts.tn}/${ts.neg} (${100 * ts.trueVeto}%.2f%%) | $covered of ${negs.size} |")
        CannotLinkRule(name, scope, atoms.map(_.condition), ts.falseVeto, ts.tn, st.upper, ts.trueVeto, ts.pos, ts.neg, "derived")
      }
    }
    val lfCatAtoms = Seq(Atom("director", Seq("different")), Atom("originalTitle", Seq("disjoint")), Atom("title", Seq("none")),
      Atom("title", Seq("none", "overlap")), Atom("country", Seq("mismatch")))
    val llCatAtoms = Seq(Atom("director", Seq("different")), Atom("originalTitle", Seq("disjoint")), Atom("title", Seq("none")),
      Atom("title", Seq("none", "overlap")), Atom("venue", Seq("same")), Atom("chainId", Seq("different")))
    val lfRules = derive(IdentityMeasures.ListingFilm, lfRows, lfCatAtoms, Seq("year.distance", "runtime.delta"), ExistingLf)
    line()
    val llRules = derive(IdentityMeasures.ListingListing, llRows, llCatAtoms, Seq("year.delta", "runtime.delta"), ExistingLl)
    line()

    // ── sensitivity: a stricter corroboration bar ──
    def sensitivity(a: Fitted, b: Fitted, name: String): Unit = {
      val diffs = a.tables.flatMap { t =>
        val o = b.tables.find(_.signal == t.signal).get.weights
        def point(ws: SignalWeights, x: Double) = ws.weight(Some(Number(x)))
        val numeric = t.weights.bins.map(bin => (s"${t.signal}[${bin.atLeast.getOrElse("")}..${bin.atMost.getOrElse("")}]", bin.weight,
          point(o, bin.atLeast.orElse(bin.atMost).getOrElse(0.0)), bin.positives + bin.negatives))
        val cats = t.weights.categories.toSeq.map { case (v, w) => (s"${t.signal}=$v", w, o.categories.getOrElse(v, 0.0),
          t.weights.counts.get(v).map(_.sum).getOrElse(0)) }
        numeric ++ cats
      }.filter(_._4 >= 30).filter { d =>
        // Only weights the stricter fit also measured on at least 30 units.
        val t = b.tables.find(tb => d._1.startsWith(tb.signal + "[") || d._1.startsWith(tb.signal + "=")).get.weights
        t.bins.exists(bin => d._1.endsWith(s"[${bin.atLeast.getOrElse("")}..${bin.atMost.getOrElse("")}]") && bin.positives + bin.negatives >= 30) ||
          t.counts.get(d._1.dropWhile(_ != '=').drop(1)).exists(_.sum >= 30) || d._1.contains("[")
      }
      if (diffs.sizeIs < 3) { line(s"- $name: fewer than 3 comparable weights"); return }
      val deltas = diffs.map(d => math.abs(d._2 - d._3))
      val (xs, ys) = (diffs.map(_._2), diffs.map(_._3))
      val (mx, my) = (xs.sum / xs.size, ys.sum / ys.size)
      val corr = xs.zip(ys).map { case (x, y) => (x - mx) * (y - my) }.sum /
        math.sqrt(xs.map(x => (x - mx) * (x - mx)).sum * ys.map(y => (y - my) * (y - my)).sum)
      val sameSign = diffs.count(d => math.signum(d._2) == math.signum(d._3))
      line(f"- $name: ${diffs.size} weights with ≥30 units; same sign ${sameSign}/${diffs.size}; mean |Δ| ${deltas.sum / deltas.size}%.3f, max |Δ| ${deltas.max}%.3f (${diffs.maxBy(d => math.abs(d._2 - d._3))._1}), correlation $corr%.4f.")
      val bySignal = diffs.groupBy(d => d._1.takeWhile(c => c != '[' && c != '=')).toSeq.sortBy(_._1).map { case (sig, ds) =>
        f"$sig ${ds.count(d => math.signum(d._2) == math.signum(d._3))}/${ds.size} same sign, mean |Δ| ${ds.map(d => math.abs(d._2 - d._3)).sum / ds.size}%.2f"
      }
      line(s"  - per signal: ${bySignal.mkString("; ")}")
      diffs.sortBy(d => -math.abs(d._2 - d._3)).take(3).foreach(d => line(f"  - ${d._1}: k=2 ${d._2}%+.2f, k=3 ${d._3}%+.2f (${d._4} units)"))
    }
    line("### Sensitivity: corroboration bar k=3 against k=2")
    line()
    sensitivity(lfFit, lfFit3, "listing ↔ film")
    sensitivity(llFit, llFit3, "listing ↔ listing")
    // The k=3 set is mostly DE/ES (they publish year, director and original title); a WITHIN-
    // country comparison separates contamination from that change of composition.
    val lf3 = lfRows(3)
    Seq("de", "us", "uk").foreach { c =>
      sensitivity(fit(IdentityMeasures.ListingFilm, LfSignals, lf2.filter(_.country == c)),
        fit(IdentityMeasures.ListingFilm, LfSignals, lf3.filter(_.country == c)), s"listing ↔ film, $c only")
    }
    line()

    // ── contradicted production filings (for healing) ──
    val contradictedTsv = new StringBuilder("country\tvenue\ttitle\tlisting year\tlisting directors\tprod film\ttmdbId\ttmdb title\ttmdb year\ttmdb directors\tdenials\tagreements\n")
    val prodFilmsAll = data.flatMap(_.prodFilms).toMap
    val detailsOf: Map[String, Int => Option[Details]] = data.map(d => d.code -> d.details).toMap
    contradicted.toSeq.sortBy(e => (obsAll(e._1).country, obsAll(e._1).listing.title)).foreach { case (i, ev) =>
      val o = obsAll(i); val d = detailsOf(o.country)(ev.tmdbId)
      contradictedTsv.append(Seq(o.country, o.venue, o.listing.rawTitle.getOrElse(o.listing.title), o.listing.statedYear.fold("")(_.toString),
        o.listing.directors.mkString(", "), o.prodFilm.getOrElse(""), ev.tmdbId.toString, d.fold("")(_.film.title),
        d.flatMap(_.film.year).fold("")(_.toString), d.flatMap(_.film.directors).getOrElse(Nil).mkString(", "),
        ev.deny.toSeq.sorted.mkString(","), ev.agree.toSeq.sorted.mkString(",")).mkString("\t")).append('\n')
    }
    // Films whose production imdbId is not TMDB's, and rating pages whose slug year is not TMDB's.
    val crossChecks = new StringBuilder("country\tprod film\ttmdbId\ttmdb year\ttmdb imdb\tprod imdb\trating url\treason\n")
    data.foreach { d =>
      d.prodFilms.values.toSeq.sortBy(_.id).foreach { f =>
        f.tmdbId.flatMap(d.details).foreach { det =>
          if (f.imdbId.isDefined && det.imdbId.isDefined && f.imdbId != det.imdbId)
            crossChecks.append(s"${d.code}\t${f.id}\t${det.id}\t${det.film.year.getOrElse("")}\t${det.imdbId.get}\t${f.imdbId.get}\t\timdb differs\n")
          f.ratingUrls.foreach { u =>
            val slugYear = """[-_](\d{4})/?$""".r.findFirstMatchIn(u).map(_.group(1).toInt)
            if (slugYear.exists(y => det.film.year.exists(ty => math.abs(ty - y) > 1)))
              crossChecks.append(s"${d.code}\t${f.id}\t${det.id}\t${det.film.year.getOrElse("")}\t${det.imdbId.getOrElse("")}\t${f.imdbId.getOrElse("")}\t$u\trating page year differs\n")
          }
        }
      }
    }
    Files.writeString(cfg.reportDir.resolve("contradicted-prod-resolutions.tsv"), contradictedTsv.toString)
    Files.writeString(cfg.reportDir.resolve("prod-cross-check-mismatches.tsv"), crossChecks.toString)
    val contradictedFilms = contradicted.values.map(e => e.tmdbId).toSet.size
    line(s"Contradicted production filings: ${contradicted.size} listings on $contradictedFilms tmdbIds (`contradicted-prod-resolutions.tsv`); " +
      s"${crossChecks.count(_ == '\n') - 1} imdb / rating-page cross-check mismatches (`prod-cross-check-mismatches.tsv`).")
    line()

    // ── held-out errors, for reading ──
    val samples = new StringBuilder("scope\tp\tlabel\tcountry\tvenue\ttitle\tother\twhy\n")
    lf2.zip(data.flatMap(_.lf)).iterator.filter(_._1.split == "test").foreach { case (r, pr) =>
      r.label(Set.empty).foreach { y =>
        val p = lfFit.probability(r.measures)
        if ((p >= 0.5) != y) {
          val o = obsAll(pr.obs)
          samples.append(s"listing-film\t${"%.4f".format(p)}\t$y\t${o.country}\t${o.venue}\t${o.listing.rawTitle.getOrElse(o.listing.title)}\ttmdb ${pr.tmdbId}\t" +
            s"${IdentityCalibration(version = "", scopes = Map(IdentityMeasures.ListingFilm -> lfFit.model)).explain(IdentityMeasures.ListingFilm, r.measures, 8)}\n")
        }
      }
    }
    ll2.zip(data.flatMap(_.ll)).iterator.filter(_._1.split == "test").foreach { case (r, pr) =>
      r.label(Set.empty).foreach { y =>
        val p = llFit.probability(r.measures)
        if ((p >= 0.5) != y) {
          val (a, b) = (obsAll(pr.a), obsAll(pr.b))
          samples.append(s"listing-listing\t${"%.4f".format(p)}\t$y\t${a.country}\t${a.venue} / ${b.venue}\t${a.listing.rawTitle.getOrElse(a.listing.title)}\t${b.listing.rawTitle.getOrElse(b.listing.title)}\t" +
            s"${IdentityCalibration(version = "", scopes = Map(IdentityMeasures.ListingListing -> llFit.model)).explain(IdentityMeasures.ListingListing, r.measures, 8)}\n")
        }
      }
    }
    Files.writeString(cfg.reportDir.resolve("held-out-errors.tsv"), samples.toString)

    val clBasis = cfg.epsilon.fold("certified: below every same-film unit of the calibration split")(e => s"same-film units below it: one-sided 95% upper bound <= $e")

    // ── the artefact ──
    val version = cfg.version
    val artefact = IdentityCalibration(
      version = version,
      scopes = Map(
        IdentityMeasures.ListingFilm -> lfFit.model.copy(thresholds = Map(
          "showRatings" -> Threshold(showT, Map("targetWrongRate" -> todayWrong,
            "heldOutWrongRate" -> (if (tn == 0) 0.0 else tw.toDouble / tn),
            "heldOutRecall" -> testShown.count(x => x._2 && x._1 >= showT).toDouble / math.max(1, testSame)),
            "lowest cut whose wrong share (labelled pairs + production's contradicted filings, per unit) on the calibration split has a one-sided 95% upper bound within today's measured wrong rate"),
          "cannotLink" -> Threshold(lfCl, lfClM, clBasis))),
        IdentityMeasures.ListingListing -> llFit.model.copy(thresholds = Map(
          "cannotLink" -> Threshold(llCl, llClM, clBasis)))),
      cannotLinks = lfRules ++ llRules,
      provenance = Map(
        "script" -> "scripts/identity-calibrate.sh (worker/Test/runMain scripts.IdentityCalibrate)",
        "corpora" -> "recorder run 36153174348: cinema-scrapes-<cc>.json.gz and enrichment-<cc> trees; hard-cluster corpora and responses",
        "production" -> cfg.prod.fold("not used")(p => s"read-only snapshot of movies + movie_slots (${p.getFileName})"),
        "labels" -> "production tmdbId corroborated by >= 2 independent signals (year ±1, director, original title, >= 2 other venues), none denying; negatives = the listing's other title-search results",
        "splits" -> "family = listings joined by title key or production film; murmur3(smallest ListingKey) mod 10: 0-4 train, 5-6 calibration, 7-9 test",
        "epsilon" -> cfg.epsilon.getOrElse((lfRules ++ llRules).map(_.falseVetoBound).maxOption.getOrElse(0.0)).toString,
        "epsilonBasis" -> cfg.epsilon.fold("certified: every rule fired on NO same-film unit of the fitting splits; epsilon is the largest Bonferroni-corrected upper bound that certifies (the tightest the data can prove)")(_ => "given on the command line"),
        "todayWrongRate" -> f"$todayWrong%.5f"))
    Files.createDirectories(cfg.weightsOut.getParent)
    Files.writeString(cfg.weightsOut, Json.prettyPrint(Json.toJson(artefact)) + "\n")

    // ── the labels ──
    Files.createDirectories(cfg.labelsOut.getParent)
    val out = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(Files.newOutputStream(cfg.labelsOut)), StandardCharsets.UTF_8))
    try {
      val labelled = evidence.filter { case (_, e) => e.positive(Set.empty, 2) || e.contradicted }.keys.toSeq.sorted
      val negatives = data.flatMap(_.lf).groupMap(_.obs)(_.tmdbId)
      val listingsJs = labelled.map { i =>
        val o = obsAll(i); val e = evidence(i)
        Json.obj("id" -> i, "country" -> o.country, "venue" -> o.venue, "listingKey" -> o.listingKey, "title" -> o.listing.title,
          "rawTitle" -> o.listing.rawTitle, "originalTitle" -> o.listing.originalTitle, "year" -> o.listing.year,
          "runtime" -> o.listing.runtime, "directors" -> o.listing.directors, "filmUrl" -> o.filmUrl,
          "family" -> familyOf(i), "split" -> split(i), "tmdbId" -> e.tmdbId,
          "status" -> (if (e.contradicted) "contradicted" else "corroborated"),
          "corroborated3" -> e.positive(Set.empty, 3),
          "agree" -> e.agree.toSeq.sorted, "deny" -> e.deny.toSeq.sorted,
          "negatives" -> (if (e.contradicted) Seq.empty[Int] else negatives.getOrElse(i, Nil).filter(_ != e.tmdbId).distinct.sorted))
      }
      // Pairs of the held-out split only: the benchmark reads nothing else, and the fitting splits'
      // pairs are regenerated by this script (they would multiply the file for no reader).
      // At most 50 per unit (pair of families), the lowest by hash: a wide release's hundred
      // thousand same-film pairs add nothing the listing labels do not already say.
      val pairsJs = ll2.zip(data.flatMap(_.ll)).filter(_._1.split == "test")
        .flatMap { case (r, p) => r.label(Set.empty).map(y => (r.unit, p, y)) }
        .groupBy(_._1).toSeq.sortBy(_._1)
        .flatMap { case (_, ps) => ps.sortBy(t => (MurmurHash3.stringHash(s"${t._2.a}|${t._2.b}"), t._2.a, t._2.b)).take(50) }
        .map { case (_, p, y) => Json.arr(p.a, p.b, y) }
      val doc = Json.obj(
        "version" -> version,
        "split" -> Json.obj("unit" -> "family: listings joined by title key or by production film (docs/design/identity-resolver.md §calibration)",
          "rule" -> "murmur3 stringHash of the family's smallest ListingKey, floorMod 10: 0-4 train, 5-6 calibration, 7-9 test",
          "benchmark" -> "use ONLY split == test"),
        "labels" -> "status corroborated: tmdbId is the film (>= 2 independent corroborators, no denial); contradicted: production's tmdbId is likely WRONG, never a positive; negatives: other films the listing's own title search returned",
        "listings" -> JsArray(listingsJs),
        "pairs" -> Json.obj("format" -> "[listing id a, listing id b, same film]; held-out split only, at most 50 per pair of families (every other pair follows from the listing labels)", "items" -> JsArray(pairsJs)))
      out.write(Json.stringify(doc))
    } finally out.close()

    val reportJs = Json.obj("listingFilm" -> lfQuality, "listingListing" -> llQuality,
      "existingVetoes" -> Json.obj("listingFilm" -> lfExisting, "listingListing" -> llExisting))
    Files.writeString(cfg.reportDir.resolve("calibration-report.json"), Json.prettyPrint(reportJs))
    Files.writeString(cfg.reportDir.resolve("calibration-report.md"), report.toString)
    println(report.toString)
    println(s"wrote ${cfg.weightsOut}, ${cfg.labelsOut}, ${cfg.reportDir}")
  }
}
