package scripts

import services.enrichment.{MovieRepo, MetacriticClient}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * For every row where `metacriticUrl` is None, probe several slug variants of
 * the title to find out which (if any) would have resolved to a canonical
 * `/movie/...` page on Metacritic. Groups results by which variant won.
 *
 * Run: sbt "Test/runMain scripts.MetacriticDiagnostics"
 */
object MetacriticDiagnostics {
  private val Site = "https://www.metacritic.com"

  private val http = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(10))
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()

  private case class Probe(label: String, slug: String, url: String, status: Int)

  private def probe(label: String, slug: String): Probe = {
    val url = s"$Site/movie/$slug/"
    val req = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(Duration.ofSeconds(15))
      .header("User-Agent", "Mozilla/5.0 (kinowo-diagnostics)")
      .GET()
      .build()
    val status = Try(http.send(req, HttpResponse.BodyHandlers.discarding()).statusCode())
      .getOrElse(-1)
    Probe(label, slug, url, status)
  }

  // Drop everything after the first colon (Polish: "Star Wars: Mandalorian" →
  // "Star Wars"). Returns the trimmed prefix, or None when there's no colon.
  private def beforeColon(t: String): Option[String] = {
    val i = t.indexOf(':'); if (i > 0) Some(t.take(i).trim) else None
  }

  // Keep only what's after the first colon ("Star Wars: Mandalorian" →
  // "Mandalorian"). MC sometimes indexes by subtitle alone.
  private def afterColon(t: String): Option[String] = {
    val i = t.indexOf(':'); if (i > 0 && i < t.length - 1) Some(t.drop(i + 1).trim) else None
  }

  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to diagnose.")
      sys.exit(1)
    }

    val rows = repo.findAll()
    val missing = rows
      .filter { case (_, _, e) => e.metacriticUrl.isEmpty }
      .sortBy { case (t, y, _) => (t.toLowerCase, y) }

    println(s"${rows.size} rows in Mongo · ${missing.size} missing Metacritic URL · probing variants…\n")

    val Workers = 4
    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done        = new AtomicInteger(0)
    val httpProbes  = new AtomicInteger(0)
    val startedAtMs = System.currentTimeMillis()

    val tasks = missing.map { case (title, year, e) =>
      Future {
        val orig      = e.originalTitle
        val linkTitle = orig.getOrElse(title)

        // Each candidate is (descriptive label, title text to slugify).
        // Order is informative — we'll pick the FIRST 200 in this order so
        // the result attributes the "easiest fix" to each row.
        val candidates: Seq[(String, String)] = Seq(
          "current (originalTitle ?? cleanTitle)" -> linkTitle,
          "cleanTitle (when different)"           -> title,
          "+ year suffix"                         -> year.map(y => s"$linkTitle $y").getOrElse(""),
          "subtitle stripped (before ':')"        -> beforeColon(linkTitle).getOrElse(""),
          "subtitle only (after ':')"             -> afterColon(linkTitle).getOrElse(""),
          "cleanTitle subtitle stripped"          -> beforeColon(title).getOrElse(""),
          "cleanTitle subtitle only"              -> afterColon(title).getOrElse("")
        ).filter(_._2.nonEmpty).distinctBy(_._2)

        val primary = MetacriticClient.slugify(linkTitle)
        val variants = candidates.map { case (label, t) => label -> MetacriticClient.slugify(t) }
        val withDeArt = variants ++ MetacriticClient
          .dropLeadingArticle(primary, '-')
          .map("de-articled (already supported)" -> _)
          .toSeq

        // Drop duplicate slugs while preserving label of FIRST winner.
        val seen = mutable.LinkedHashMap.empty[String, String]
        withDeArt.foreach { case (label, slug) => if (!seen.contains(slug)) seen.put(slug, label) }

        val probes = seen.iterator.map { case (slug, label) =>
          httpProbes.incrementAndGet()
          probe(label, slug)
        }.toList

        val finishedIdx = done.incrementAndGet()
        if (finishedIdx % 20 == 0) println(s"  … $finishedIdx/${missing.size}")
        (title, year, e.originalTitle, probes)
      }
    }

    val all = Await.result(Future.sequence(tasks), 10.minutes)
    pool.shutdown()

    // Group: which label "won" (first 200)?
    val grouped = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[(String, Option[Int], Option[String], Probe)]]
    val unresolved = mutable.ArrayBuffer.empty[(String, Option[Int], Option[String])]
    all.foreach { case (t, y, o, probes) =>
      probes.find(_.status == 200) match {
        case Some(win) =>
          grouped.getOrElseUpdate(win.label, mutable.ArrayBuffer.empty).append((t, y, o, win))
        case None =>
          unresolved.append((t, y, o))
      }
    }

    println("\n══════════════════════════════════════════════════════")
    println("Groups by which variant resolved to a canonical /movie/ URL")
    println("══════════════════════════════════════════════════════\n")
    grouped.foreach { case (label, rows) =>
      println(s"── $label  (${rows.size}) ──")
      rows.foreach { case (t, y, o, win) =>
        val origStr = o.fold("")(s => s" [orig=$s]")
        println(f"  $t (${y.getOrElse("?")})$origStr → ${win.url}")
      }
      println()
    }

    println("══════════════════════════════════════════════════════")
    println(s"Unresolved by any variant (${unresolved.size})")
    println("══════════════════════════════════════════════════════\n")
    unresolved.foreach { case (t, y, o) =>
      val origStr = o.fold("")(s => s" [orig=$s]")
      println(s"  $t (${y.getOrElse("?")})$origStr")
    }

    val elapsedSec = (System.currentTimeMillis() - startedAtMs) / 1000.0
    val rps        = if (elapsedSec > 0) f"${httpProbes.get() / elapsedSec}%.1f" else "—"
    println(f"\nDone in $elapsedSec%.1fs · ${httpProbes.get()} MC probes total · ~$rps req/s across $Workers workers.")

    repo.close()
  }
}
