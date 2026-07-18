package clients.tools

import tools.{HttpFetch, RealHttpFetch}
import models._
import services.cinemas._
import services.cinemas.common.CinemaScraper
import services.cinemas.pl._

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/** Live burst probe. Wraps `RealHttpFetch` to measure, per cinema client,
 *  how aggressively it hits the real site: total requests, the MAX number
 *  of requests in flight at once (the "hitting too fast" signal), and any
 *  non-2xx / timeout the site pushed back with.
 *
 *  Run: `sbt 'test/runMain clients.tools.ProbeBurst [name ...]'`
 *  With no args it runs the whole curated list. Each client gets its own
 *  fetch instance so the burst metric is per-client. Clients run
 *  sequentially with a short pause between them — this mirrors how prod
 *  scrapes each cinema independently and keeps us from hammering several
 *  hosts at once from one IP.
 */
object ProbeBurst {

  /** Per-client instrumented fetch. `inFlight` is bumped when an async GET
   *  starts and dropped when it completes; `maxInFlight` is the high-water
   *  mark — that's the burst size the site sees. Blocking `get` is counted
   *  but (being synchronous) only ever contributes 1 to in-flight. */
  class Instrumented(delegate: RealHttpFetch) extends HttpFetch {
    val total       = new AtomicInteger(0)
    val inFlight    = new AtomicInteger(0)
    val maxInFlight = new AtomicInteger(0)
    val failures    = mutable.Buffer[String]()

    private def bump(): Unit = {
      val now = inFlight.incrementAndGet()
      maxInFlight.updateAndGet(m => math.max(m, now))
    }
    private def record(url: String, t: Throwable): Unit = synchronized {
      val cause = Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
      failures += s"${host(url)}: $cause"
    }

    override def get(url: String): String = {
      total.incrementAndGet(); bump()
      try delegate.get(url)
      catch { case t: Throwable => record(url, t); throw t }
      finally inFlight.decrementAndGet()
    }

    override def getAsync(url: String): CompletableFuture[String] = {
      total.incrementAndGet(); bump()
      delegate.getAsync(url).whenComplete { (_, t) =>
        inFlight.decrementAndGet()
        if (t != null) record(url, t)
      }
    }

    override def post(url: String, body: String, contentType: String): String = {
      total.incrementAndGet(); bump()
      try delegate.post(url, body, contentType)
      catch { case t: Throwable => record(url, t); throw t }
      finally inFlight.decrementAndGet()
    }
  }

  private def host(url: String): String =
    try new java.net.URI(url).getHost catch { case _: Throwable => url.take(30) }

  /** name -> build a scraper over the given fetch. */
  private val builders: Map[String, HttpFetch => CinemaScraper] = Map(
    "rialto"        -> (f => new RialtoClient(f)),
    "nowe-horyzonty"-> (f => new NoweHoryzontyClient(f)),
    "dcf"           -> (f => new DcfClient(f)),
    "cytadela"      -> (f => new CytadelaClient(f)),
    "muranow"       -> (f => new MuranowClient(f)),
    "iluzjon"       -> (f => new IluzjonClient(f)),
    "amondo"        -> (f => new AmondoClient(f)),
    "kinomuzeum"    -> (f => new KinomuzeumClient(f)),
    "kinoteka"      -> (f => new KinotekaClient(f)),
    "ujazdowski"    -> (f => new UjazdowskiClient(f)),
    "kino-kultura"  -> (f => new KinoKulturaClient(f)),
    "falenica"      -> (f => new FalenicaClient(f)),
    "sdk"           -> (f => new SdkClient(f)),
    "prom-kepa"     -> (f => new PromKepaClient(f)),
    "swit"          -> (f => new SwitClient(f)),
    "nove-kino"     -> (f => new NoveKinoClient(f, "atlantic", KinoAtlantic)),
    "luna"          -> (f => new Bilety24Client(f, "https://kinoluna.bilety24.pl", KinoLuna)),
    "elektronik"    -> (f => new Bilety24OrganizerClient(f, "https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik)),
    "na-boku"       -> (f => new BokClient(f, "kino-na-boku", KinoNaBoku)),
    "glebocka"      -> (f => new BokClient(f, "kino-glebocka-66", KinoGlebocka66)),
    "kinogram"      -> (f => new KinoGramClient(f))
  )

  def main(args: Array[String]): Unit = {
    val names = if (args.nonEmpty) args.toSeq else builders.keys.toSeq.sorted
    val rows  = mutable.Buffer[(String, Int, Int, Int, Long, Int)]()
    for (name <- names) {
      builders.get(name) match {
        case None => println(s"!! unknown client: $name")
        case Some(build) =>
          val fetch = new Instrumented(new RealHttpFetch())
          val t0    = System.currentTimeMillis()
          val movies = try build(fetch).fetch() catch {
            case t: Throwable => println(s"$name: fetch threw ${t.getClass.getSimpleName}: ${t.getMessage}"); Seq.empty
          }
          val ms = System.currentTimeMillis() - t0
          rows += ((name, fetch.total.get, fetch.maxInFlight.get, fetch.failures.size, ms, movies.size))
          println(f"$name%-15s reqs=${fetch.total.get}%-4d maxBurst=${fetch.maxInFlight.get}%-4d fails=${fetch.failures.size}%-3d ${ms}%5dms movies=${movies.size}")
          fetch.failures.take(8).foreach(x => println(s"      ! $x"))
          Thread.sleep(1500) // politeness pause between hosts
      }
    }
    println("\n==== SUMMARY (sorted by burst) ====")
    println(f"${"client"}%-15s ${"reqs"}%5s ${"burst"}%6s ${"fails"}%6s ${"ms"}%7s ${"movies"}%7s")
    rows.sortBy(-_._3).foreach { case (n, t, b, f, ms, mv) =>
      println(f"$n%-15s $t%5d $b%6d $f%6d $ms%7d $mv%7d")
    }
  }
}
