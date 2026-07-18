package scripts

import tools.{HttpFetch, RealHttpFetch, ResidentialProxy, SessionWarmingHttpFetch}
import services.cinemas.pl.MultikinoClient

/**
 * Reproduce + isolate the intermittent Multikino-via-proxy 401 (the
 * `IOException: WWW-Authenticate header missing` seen in prod). Hammers every
 * Multikino location's API through the REAL java.net.http proxied client — the
 * thing curl can't mimic, because the JVM's persistent CookieManager accumulates
 * Multikino's session cookie across the burst + rotating IPs.
 *
 *   KINOWO_PROXY_USER=… KINOWO_PROXY_PASS=… sbt 'worker/Test/runMain scripts.MultikinoProxyDiag'
 */
object MultikinoProxyDiag {
  // Warm-required locations FIRST (0015/0042/0008/0041 401 on a cold call), to
  // exercise the empty-jar edge case the SessionWarming fix must rescue.
  private val Ids = Seq(
    "0015","0042","0008","0041","0011","0010","0013","0040","0052","0024","0025",
    "0005","0023","0035","0007","0004","0006","0034","0026","0029","0028","0003",
    "0036","0014","0047","0037","0053","0050","0030","0027","0033","0043")

  def main(args: Array[String]): Unit = ResidentialProxy.fromEnv() match {
    case None => println("set KINOWO_PROXY_USER / KINOWO_PROXY_PASS")
    case Some(cfg) =>
      val rounds = if (args.nonEmpty) args(0).toInt else 2
      run("single-IP, NO warm (some cinemas 401 cold)", new RealHttpFetch(Some(cfg)), rounds)
      run("single-IP + SessionWarming (the fix)",
        new SessionWarmingHttpFetch(new RealHttpFetch(Some(cfg)), MultikinoClient.HomeUrl), rounds)
  }

  private def run(label: String, fetch: HttpFetch, rounds: Int): Unit = {
    var ok = 0; var fail = 0
    val errs = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    for (_ <- 1 to rounds; id <- Ids) {
      try {
        val body = fetch.get(MultikinoClient.apiUrl(id))
        if (body.contains("\"result\"")) ok += 1
        else { fail += 1; bump(errs, s"200-but-no-result(${body.length}b)") }
      } catch {
        case e: Throwable => fail += 1
          bump(errs, s"${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("").take(70)}")
      }
    }
    println(s"\n[$label] ${Ids.size * rounds} requests → ok=$ok fail=$fail")
    errs.toSeq.sortBy(-_._2).foreach { case (k, n) => println(f"  $n%3d  $k") }
  }

  private def bump(m: scala.collection.mutable.Map[String, Int], k: String): Unit =
    m.update(k, m.getOrElse(k, 0) + 1)
}
