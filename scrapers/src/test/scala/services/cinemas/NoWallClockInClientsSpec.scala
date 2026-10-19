package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** Regression guard for the "wall-clock leak": a cinema scraper must derive
 *  every date from the injected `today` (or a passed-in `fallbackYear`), never
 *  from `LocalDate.now()` / `LocalDateTime.now()` read inside its logic.
 *  When the real calendar date drifts from a recorded fixture's capture date,
 *  any client that reads the wall clock diverges and its fixture lookups miss —
 *  which is what made `FilmScheduleEndToEndSpec` fail once the date advanced.
 *
 *  No `.now(` is permitted, not even as a parameter default: `today` and
 *  `fallbackYear` are required parameters, so the compiler makes every
 *  construction site (`CinemaScraperCatalog`, specs) hand the injected date in.
 *  Defaults once let `EkobiletClient`, `KinoIkm`, `KinoZak` and `KinoZorza` be
 *  built without `today`, drifting the whole-corpus snapshot day by day. */
class NoWallClockInClientsSpec extends AnyFlatSpec with Matchers {

  private val ClientsDirectory: Path = Paths.get("scrapers/src/main/scala/services/cinemas")

  private val NowCall = """Local(?:Date|DateTime)\.now\(""".r

  "Cinema scraper clients" should "never read the wall clock" in {
    Files.exists(ClientsDirectory) shouldBe true

    val offenders: Seq[String] =
      Files.walk(ClientsDirectory).iterator.asScala.toSeq
        .filter(p => p.getFileName.toString.endsWith(".scala"))
        .sortBy(_.toString)
        .flatMap { path =>
          Files.readAllLines(path).asScala.zipWithIndex.collect {
            case (line, index) if NowCall.findFirstIn(line).isDefined =>
              s"$path:${index + 1}: ${line.trim}"
          }
        }

    withClue(
      "These cinema-client lines read the wall clock instead of the injected `today`/`fallbackYear` " +
        "(this reopens the fixture wall-clock leak):\n" + offenders.mkString("\n") + "\n"
    ) {
      offenders shouldBe empty
    }
  }
}
