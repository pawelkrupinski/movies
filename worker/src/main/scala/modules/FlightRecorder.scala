package modules

import settings.HeapDumpDirectory

import java.time.{Clock, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.FiniteDuration

/** A profile of the running worker, on demand — what `/profile` starts. */
trait FlightRecorder {
  /** Start recording for `duration`, written when it ends; the file it will be, or why not. */
  def record(duration: FiniteDuration): Either[String, String]
}

/**
 * A Java Flight Recorder recording, with the JDK's `profile` settings (CPU samples, allocation
 * samples, locks, GC), into the heap-dump directory — the node's persisted volume, where it can be
 * copied off like a dump. The worker image is JRE-only, so nothing can attach from outside to start
 * one; this is the only way to see where a production worker's time and allocation go (a local
 * mirror reproduces one pass, not a busy worker's day).
 *
 * One recording at a time: a second is refused while the first runs.
 */
final class JfrFlightRecorder(dir: HeapDumpDirectory, clock: Clock = Clock.systemUTC()) extends FlightRecorder {
  private var running: Option[jdk.jfr.Recording] = None

  def record(duration: FiniteDuration): Either[String, String] = synchronized {
    if (running.exists(_.getState == jdk.jfr.RecordingState.RUNNING)) Left("a recording is already running")
    else {
      java.nio.file.Files.createDirectories(dir.value)
      val stamp     = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").withZone(ZoneOffset.UTC).format(clock.instant())
      val file      = dir.value.resolve(s"profile-$stamp.jfr")
      val recording = new jdk.jfr.Recording(jdk.jfr.Configuration.getConfiguration("profile"))
      recording.setName("kinowo-worker-profile")
      recording.setToDisk(true)
      recording.setDestination(file)
      recording.setDuration(java.time.Duration.ofMillis(duration.toMillis))
      recording.start()
      running = Some(recording)
      Right(file.toString)
    }
  }
}
