package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Regression guard for the "wall-clock leak": a cinema scraper must derive
 *  every date from the injected `today` (or a passed-in `fallbackYear`), never
 *  from `LocalDate.now()` / `LocalDateTime.now()` read live inside its logic.
 *  When the real calendar date drifts from a recorded fixture's capture date,
 *  any client that reads the wall clock diverges and its fixture lookups miss —
 *  which is what made `FilmScheduleEndToEndSpec` fail once the date advanced.
 *
 *  The ONLY permitted `.now(` in these files is a constructor/method default
 *  parameter — `today: LocalDate = LocalDate.now(...)` or
 *  `fallbackYear: Int = LocalDate.now(...).getYear` — because the composition
 *  root (`CinemaScraperCatalog`) always overrides it with the injected `today`;
 *  the default merely keeps a stand-alone diagnostic constructible. Every other
 *  `.now(` is a leak and fails this spec, naming the offending file:line. */
class NoWallClockInClientsSpec extends AnyFlatSpec with Matchers {

  private val ClientsDir: Path = Paths.get("worker/src/main/scala/services/cinemas")

  private val NowCall = """Local(?:Date|DateTime)\.now\(""".r

  // A `.now(` that sits on a default-parameter declaration is allowed: either a
  // `today: LocalDate = LocalDate.now(...)` ctor/method default, or a
  // `fallbackYear ... = LocalDate.now(...)` default. Both are overridden by the
  // injected `today` at every production call site.
  private val AllowedDefault =
    """(?:today|fallbackYear)\s*:?\s*(?:LocalDate|Int)?\s*=\s*(?:java\.time\.)?LocalDate\.now\(""".r

  "Cinema scraper clients" should "never read the wall clock outside a default-parameter declaration" in {
    Files.exists(ClientsDir) shouldBe true

    val offenders: Seq[String] =
      Files.list(ClientsDir).iterator.asScala.toSeq
        .filter(p => p.getFileName.toString.endsWith(".scala"))
        .sortBy(_.getFileName.toString)
        .flatMap { path =>
          Files.readAllLines(path).asScala.zipWithIndex.collect {
            case (line, idx)
                if NowCall.findFirstIn(line).isDefined && AllowedDefault.findFirstIn(line).isEmpty =>
              s"${path.getFileName}:${idx + 1}: $line"
          }
        }

    withClue(
      "These cinema-client lines read the wall clock instead of the injected `today`/`fallbackYear` " +
        "(this reopens the fixture wall-clock leak):\n" + offenders.mkString("\n") + "\n"
    ) {
      offenders shouldBe empty
    }
  }

  // The spec above only proves a client doesn't read the clock OUTSIDE a default
  // parameter; it trusts the composition root to OVERRIDE that default with the
  // injected `today`. That trust was misplaced — `EkobiletClient` (Kino Meduza,
  // …) and later `KinoIkm`/`KinoZak`/`KinoZorza` were constructed in
  // `CinemaScraperCatalog` WITHOUT `today`, so they resolved screening dates
  // against the real calendar and the whole-corpus snapshot drifted day to day
  // ("Bez wyjścia"'s 10/11-June screenings fell out of the fixture window once
  // the real date passed them). Verify the override for EVERY date-sensitive
  // client, so a new venue can't silently reopen the leak.
  it should "pin the injected `today` on every date-sensitive client in CinemaScraperCatalog" in {
    val catalog = Paths.get("worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala")
    Files.exists(catalog) shouldBe true
    val src = new String(Files.readAllBytes(catalog), java.nio.charset.StandardCharsets.UTF_8)

    // Clients that default `today`/`fallbackYear` to the wall clock — they MUST
    // be handed the injected date wherever the catalog builds them directly.
    val dateSensitive: Seq[String] =
      Files.list(ClientsDir).iterator.asScala.toSeq
        .filter(p => p.getFileName.toString.endsWith("Client.scala"))
        .filter(p => AllowedDefault.findFirstIn(new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8)).isDefined)
        .map(_.getFileName.toString.stripSuffix(".scala"))
        .sorted

    val offenders: Seq[String] = dateSensitive.flatMap { client =>
      // Direct `new XClient(...)` sites in the catalog (single-line; multi-line
      // venues pass `today` explicitly already). Constructed elsewhere (Helios /
      // Filmweb in WorkerWiring, where `today` is passed) yields no match here.
      s"""new $client\\([^)]*\\)""".r.findAllIn(src).toSeq
        .filterNot(ctor => ctor.contains("today") || ctor.contains("fallbackYear"))
        .map(ctor => s"  $ctor")
    }
    withClue(s"Date-sensitive client(s) built without the injected `today` (wall-clock leak):\n${offenders.mkString("\n")}\n") {
      offenders shouldBe empty
    }
  }
}
