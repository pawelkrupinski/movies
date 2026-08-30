package clients.tools

import models.UsRoster
import services.cinemas.common.GatsbyBoxOfficeClient
import services.cinemas.us.AlamoDrafthouseClient
import tools.RealHttpFetch

import java.time.{LocalDate, ZoneId}

/**
 * Record the live US chain responses as fixtures for `AlamoDrafthouseClientSpec`,
 * `ShowcaseUsClientSpec` and `LandmarkClientSpec` to replay.
 *
 * Run with `sbt "worker/Test/runMain clients.tools.RecordUsChains"`. Pass a date
 * as the single argument to pin `today` (the Webedia schedule URL carries
 * `from`/`to`, so its fixture filename is date-dependent and the spec pins the
 * same day); with no argument it records for today and the specs must be updated
 * to match.
 *
 * Paced by `RealHttpFetch`'s own per-host rows — `drafthouse.com`,
 * `showcasecinemas.com` and `landmarktheatres.com` each earn one, so this
 * recorder cannot out-run the origins even though it is a burst of one-off calls.
 */
object RecordUsChains {
  def main(args: Array[String]): Unit = {
    val today = args.headOption.map(LocalDate.parse).getOrElse(LocalDate.now(ZoneId.of("America/Chicago")))
    println(s"recording for today=$today")

    def cinema(name: String) =
      UsRoster.byDisplayName.getOrElse(name, sys.error(s"no US roster venue named '$name'"))

    // Alamo — Lakeline (Austin, 300 sessions over 44 days when captured).
    val alamo = new AlamoDrafthouseClient(
      new RecordingHttpFetch("alamo-drafthouse", new RealHttpFetch()),
      "lakeline", cinema("Alamo Drafthouse Lakeline"), ZoneId.of("America/Chicago"), today = Some(today))
    report("Alamo Lakeline", alamo.fetch())

    // Showcase US — Legacy Place (Dedham MA), the deepest-horizon venue of the 13.
    val showcase = new GatsbyBoxOfficeClient(
      new RecordingHttpFetch("showcase-us", new RealHttpFetch()),
      GatsbyBoxOfficeClient.ShowcaseUsBaseUrl, "X0C11", cinema("Showcase Legacy Place Dedham"),
      timeZone = "America/New_York",
      venuePath = Some("/theaters/x0c11-showcase-cinema-de-lux-legacy-place"),
      today = today)
    report("Showcase Legacy Place", showcase.fetch())

    // Landmark — Nuart (West LA), a single-screen repertory house: the shape most
    // exposed to a short horizon, and the one whose own feed beat flicks.us by the
    // widest margin (32 advertised days against 27).
    val landmark = new GatsbyBoxOfficeClient(
      new RecordingHttpFetch("landmark", new RealHttpFetch()),
      GatsbyBoxOfficeClient.LandmarkBaseUrl, "X00CW", cinema("Landmark Nuart Theatre"),
      timeZone = "America/Los_Angeles",
      venuePath = Some("/theaters/x00cw-landmark-nuart-theatre-west-los-angeles"),
      today = today)
    report("Landmark Nuart", landmark.fetch())

    // An IDLE venue, captured for real rather than hand-emptied: ask the same
    // endpoint for a window years past anything on sale and the platform answers
    // with its genuine "nothing scheduled" body. That is a real recorded response
    // to a real request, which a payload we edited ourselves would not be.
    //
    // Recorded into the SAME fixture directory as the populated window: the
    // schedule URL carries `from`/`to`, so the two windows are distinct fixture
    // keys, and the chain catalogue (340KB / 700KB) is then shared rather than
    // checked in twice.
    val idleDay = LocalDate.of(2029, 1, 1)
    val showcaseIdle = new GatsbyBoxOfficeClient(
      new RecordingHttpFetch("showcase-us", new RealHttpFetch()),
      GatsbyBoxOfficeClient.ShowcaseUsBaseUrl, "X0C11", cinema("Showcase Legacy Place Dedham"),
      timeZone = "America/New_York", today = idleDay)
    report("Showcase (idle window)", showcaseIdle.fetch())

    val landmarkIdle = new GatsbyBoxOfficeClient(
      new RecordingHttpFetch("landmark", new RealHttpFetch()),
      GatsbyBoxOfficeClient.LandmarkBaseUrl, "X00CW", cinema("Landmark Nuart Theatre"),
      timeZone = "America/Los_Angeles", today = idleDay)
    report("Landmark (idle window)", landmarkIdle.fetch())
  }

  private def report(label: String, films: Seq[models.CinemaMovie]): Unit = {
    val showtimes = films.flatMap(_.showtimes)
    val days      = showtimes.map(_.dateTime.toLocalDate).distinct.sorted
    println(f"$label%-24s ${films.size}%3d films, ${showtimes.size}%5d showtimes, " +
      f"${days.size}%3d days ${days.headOption.getOrElse("-")} .. ${days.lastOption.getOrElse("-")}")
  }
}
