package scripts

import services.identity._
import services.movies.{ListingKey, TitleNormalizer}
import tools.UnmatchedClusters

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * One country's unmatched-cluster capture ([[UnmatchedClusters]]) re-resolved under a measure's change — a title rule
 * ([[DecorationDiscovery]]) or a calibration ([[IdentityRefit]]) — with the agreement stage replayed over the result. A
 * TMDB question the capture holds no answer to is asked `live` when a key is configured (each answer kept for the next
 * measure), and counted as a gap otherwise: a measure with more gaps than its control is unmeasured, never guessed.
 */
final class CaptureReplay(val capture: UnmatchedClusters.Capture, live: Option[IdentityLookups]) {
  import CaptureReplay._

  val normalizer: TitleNormalizer = TitleNormalizer.forCountry(capture.country)
  private val queries = new ConcurrentHashMap[CandidateQuery, Seq[Hit]](capture.queries.asJava)
  private val films   = new ConcurrentHashMap[Int, Option[IdentityMeasures.Film]](capture.films.asJava)

  private def answered: UnmatchedClusters.Capture = capture.copy(queries = queries.asScala.toMap, films = films.asScala.toMap)

  /** `measure` over the capture as answered so far, the TMDB questions it left open asked live and the measure run again
   *  while any is (at most five times): its takes, and how many questions stayed unanswered. */
  private def answering(measure: (UnmatchedClusters.Capture, UnmatchedClusters.Replay) => (Seq[UnmatchedClusters.Take], Set[CandidateQuery], Set[Int])):
      (Seq[UnmatchedClusters.Take], Int) = {
    var rounds = 0
    var result = (Seq.empty[UnmatchedClusters.Take], Int.MaxValue)
    var done   = false
    while (!done) {
      val cap     = answered
      val lookups = new UnmatchedClusters.Replay(cap)
      val (takes, outcomeQueries, outcomeFilms) = measure(cap, lookups)
      val missingQ = lookups.queries.asScala.toSet ++ outcomeQueries
      val missingF = lookups.films.asScala.toSet.map(_.toInt) ++ outcomeFilms
      result = (takes, missingQ.size + missingF.size)
      rounds += 1
      done = result._2 == 0 || live.isEmpty || rounds > 4
      live.filterNot(_ => done).foreach { tmdb =>
        asked(missingQ.toSeq.map(q => () => tmdb.candidates(q).toOption.foreach(queries.put(q, _))) ++
          missingF.toSeq.map(id => () => tmdb.film(id).toOption.foreach(films.put(id, _))))
      }
    }
    result
  }

  /** The `touched` clusters resolved again over `listings` under `n`, the rest as captured; the agreement replayed over
   *  all of them: the takes, and how many TMDB questions stayed unanswered. */
  def replay(listings: Seq[Listing], touched: Set[ResolverDecision], n: TitleNormalizer = normalizer): (Seq[UnmatchedClusters.Take], Int) =
    answering { (answeredCapture, lookups) =>
      val cap      = answeredCapture.copy(listings = listings)
      val keys     = touched.flatMap(_.members)
      val resolved = IdentityResolver.resolve(listings.filter(l => keys(l.key)), lookups, n)
      val outcome  = UnmatchedClusters.replay(cap, capture.decisions.filterNot(touched) ++ resolved.decisions, normalizer = Some(n))
      (UnmatchedClusters.takes(cap, outcome), outcome.missingQueries, outcome.missingFilms)
    }

  /** Every listing of the capture resolved again under `calibration` — alone, without the rest of the corpus its
   *  captured decisions were made beside, so only compared with another such resolve ([[spliced]]). */
  def resolved(calibration: IdentityCalibration): Seq[ResolverDecision] =
    IdentityResolver.resolve(capture.listings, new UnmatchedClusters.Replay(answered), normalizer, calibration).decisions

  /** A calibration measured as the ratchet measures it: `base` (the captured decisions, else those a kept change left)
   *  with every cluster the capture's resolve under `calibration` decides otherwise than under the control (`was`) taken
   *  from that resolve, and the agreement replayed under `calibration` — the takes, the gaps, the decisions and the
   *  resolve, to measure the next change on top of this one. */
  def measure(base: Seq[ResolverDecision], was: Seq[ResolverDecision], calibration: IdentityCalibration): Measure = {
    var decisions = base
    var now       = was
    val (takes, gaps) = answering { (cap, lookups) =>
      now       = IdentityResolver.resolve(capture.listings, lookups, normalizer, calibration).decisions
      decisions = spliced(base, was, now)
      val outcome = UnmatchedClusters.replay(cap, decisions, calibration = calibration)
      (UnmatchedClusters.takes(cap, outcome), outcome.missingQueries, outcome.missingFilms)
    }
    Measure(takes, gaps, decisions, now)
  }
}

object CaptureReplay {
  /** How many TMDB questions are in flight at once, across every capture and measure: TMDB allows ~50 requests a
   *  second, and a question is one or two round trips (the external-api-rate-limits skill: 10 workers is fine). */
  val AskConcurrency = 8
  private lazy val askPool = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(AskConcurrency, (r: Runnable) => { val t = new Thread(r, "tmdb-ask"); t.setDaemon(true); t })
    scala.concurrent.ExecutionContext.fromExecutor(pool)
  }

  /** Every ask run, [[AskConcurrency]] at a time, returning once all have: a control's hundreds of unanswered questions
   *  asked one after another were most of a CI run's control (1,308 of them, ~9 minutes, 2026-10-15). */
  private[scripts] def asked(asks: Seq[() => Unit]): Unit = {
    import scala.concurrent.{Await, Future}
    given scala.concurrent.ExecutionContext = askPool
    Await.result(Future.traverse(asks)(ask => Future(ask())), scala.concurrent.duration.Duration.Inf)
  }

  /** A calibration's measure over one capture ([[CaptureReplay.measure]]). */
  final case class Measure(takes: Seq[UnmatchedClusters.Take], gaps: Int, decisions: Seq[ResolverDecision], resolved: Seq[ResolverDecision])

  /** What of a decision the agreement stage and the takes read — never its confidence or its explanation, which every
   *  reweighing moves for a cluster deciding the same. */
  private def outcome(d: ResolverDecision) = (d.listings, d.film, d.basis, d.fallback, d.leaning, d.candidate)

  /** `base` with the clusters `now` decides otherwise than `was` replaced by `now`'s: the listings of every decision of
   *  `now` that `was` holds no equal of, closed over the clusters of all three partitions sharing a listing with them —
   *  so a cluster `now` splits or joins is replaced whole — and every other decision of `base` kept as it was. The
   *  closure is the connected components of the three partitions' listings, joined by a cluster: one union-find pass,
   *  where it was a repeat-until-stable scan of every decision per round. */
  def spliced(base: Seq[ResolverDecision], was: Seq[ResolverDecision], now: Seq[ResolverDecision]): Seq[ResolverDecision] = {
    val same    = was.map(outcome).toSet
    val changed = now.filterNot(d => same(outcome(d))).flatMap(_.members)
    if (changed.isEmpty) base else {
      val parent = scala.collection.mutable.HashMap.empty[ListingKey, ListingKey]
      def find(key: ListingKey): ListingKey = {
        var root = parent.getOrElseUpdate(key, key)
        while (parent(root) != root) root = parent(root)
        var at = key
        while (parent(at) != root) { val next = parent(at); parent(at) = root; at = next }
        root
      }
      (base ++ was ++ now).foreach(d => d.members.headOption.foreach(first => d.members.foreach(key => parent(find(key)) = find(first))))
      val reached = changed.map(find).toSet
      val touched = (d: ResolverDecision) => d.members.exists(key => reached(find(key)))
      base.filterNot(touched) ++ now.filter(touched)
    }
  }

  /** Every country's checked-in capture, each asking its TMDB gaps live when `TMDB_API_KEY` is configured. */
  def all(): Seq[CaptureReplay] = {
    val configuration = settings.ProcessConfiguration.resolve()
    models.Country.all.map(UnmatchedClusters.fixturePath).filter(java.nio.file.Files.exists(_)).map(UnmatchedClusters.read).map { capture =>
      val live = configuration.tmdbApiKey.map(key => new TmdbIdentityLookups(new clients.TmdbClient(new tools.RealHttpFetch(), apiKey = Some(key),
        // a real backoff on a 429 or a 5xx: asked AskConcurrency at a time, a retry at once would meet the same limit
        language = capture.country.language), new services.enrichment.ImdbClient(new tools.RealHttpFetch()), Nil))
      new CaptureReplay(capture, live)
    }
  }
  def asksLive: Boolean = settings.ProcessConfiguration.resolve().tmdbApiKey.isDefined
}
