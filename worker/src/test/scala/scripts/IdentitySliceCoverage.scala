package scripts

import tools.SliceHttp

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * SLICE ANSWER COVERAGE: every pinned slice -- the identity slice cases (`test/resources/fixtures/identity-slices/`,
 * IdentitySliceCasesSpec) and the drift slices (`test/resources/fixtures/drift-slices/<cc>/`, IdentityDriftSlicesSpec) --
 * decided again at this checkout's code from its own answers alone, and EVERY request a slice asks that it does not hold
 * named, all slices at once. A resolver, title-rule or shape change that makes a listing ask a new search fails
 * IdentitySliceCasesSpec with "lacks answers" for the first five of the first case only; Unmatched C's banner shapes turned
 * main red twice (#2180: the Halstead and Keswick cases) as each missing answer was found one push at a time.
 *
 *   IdentitySliceCoverage [--slices DIR]... [--repin] [--recordings DIR] [--needs FILE] [--report FILE]
 *
 * `--slices`: the slice directories (a case or drift slice: holds `slice.json.gz`), or roots holding them; default both
 * fixture roots. `--report FILE`: every line said, kept there too. `--needs FILE`: each lacking slice's recording, `<key>\t<cc>` a line (the cache key under
 * `<recordings>/<key>/pair/test/resources/fixtures`), for the wrapper to fetch before `--repin`.
 *
 * `--repin`: each lacking slice topped up from ITS OWN recording (the one it was cut from, `slice.json.gz`'s `recording`,
 * found under `--recordings`: the wrapper passes `$KINOWO_IDENTITY_CACHE/recordings`): decided again with the recording behind it
 * ([[SliceHttp.Replaying]]'s widen), every answer it lacked noted from the recording, and its `http.jsonl.gz` written
 * with them -- round after round until it decides alone lacking nothing. Only the answers move: its listings, context,
 * filed answers and `expected.tsv` stay as pinned (scripts/identity-slice-refresh re-cuts a case whole).
 *
 * It runs inside the checkout's kept sbt server (`runMain` is not forked there), so it never calls `sys.exit` -- that ended
 * the server: a failure is [[Failed]] thrown, the run's task failing. Succeeds when every slice holds every answer it asks
 * (or, `--repin`, every lacking one was re-pinned).
 * The pre-push hook's identity area runs it first (scripts/hooks/local-tests.txt, scripts/identity-slice-coverage).
 */
object IdentitySliceCoverage {

  val Roots: Seq[Path] = Seq(Path.of("test", "resources", "fixtures", "identity-slices"), Path.of("test", "resources", "fixtures", "drift-slices"))

  /** One slice's replay: the requests it asked and does not hold, or why it could not be decided. */
  final case class Coverage(dir: Path, recording: String, country: String, lacking: Seq[String], failed: Option[String]) {
    def covered: Boolean = lacking.isEmpty && failed.isEmpty
  }

  /** The slice directories under `paths`: each one holding `slice.json.gz`, or every one below it. */
  def slicesUnder(paths: Seq[Path]): Seq[Path] = paths.filter(Files.isDirectory(_)).flatMap { p =>
    if (Files.exists(p.resolve("slice.json.gz"))) Seq(p)
    else {
      val walk = Files.walk(p)
      try walk.iterator().asScala.filter(d => Files.isDirectory(d) && Files.exists(d.resolve("slice.json.gz"))).toSeq finally walk.close()
    }
  }.distinct.sortBy(_.toString)

  def coverageOf(dir: Path): Coverage = Try(IdentitySlice.readSlice(dir)) match {
    case Failure(e) => Coverage(dir, "", "", Nil, Some(s"unreadable: $e"))
    case Success(slice) =>
      Try(IdentitySlice.decidePinned(slice)) match {
        case Success((_, _, replaying)) => Coverage(dir, slice.recording, slice.country.code, rendered(replaying.lacking.asScala), None)
        case Failure(e)                 => Coverage(dir, slice.recording, slice.country.code, Nil, Some(s"could not be decided: $e"))
      }
  }

  private def rendered(requests: Iterable[SliceHttp.Request]): Seq[String] = requests.map(_.render).toSeq.sorted

  /** Every slice of `dirs` replayed, side by side (each decides from its own answers and shares nothing). */
  def coverage(dirs: Seq[Path]): Seq[Coverage] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors))
    try ParallelTasks.all(dirs)(coverageOf)(using scala.concurrent.ExecutionContext.fromExecutor(pool))
    finally pool.shutdown()
  }

  /** The recording cache key a slice names: `<cache>/recordings/<key>/pair/test/resources/fixtures`. */
  def recordingKey(recording: String): Option[String] =
    """(?:^|/)recordings/([^/]+)/pair/test/resources/fixtures/?$""".r.findFirstMatchIn(recording).map(_.group(1))

  /** Where a re-pin reads what a slice lacks: a fetch over its recording, noting each outcome. */
  type RecordingOf = IdentitySlice.Slice => SliceHttp.Noting

  /** The slice's own recording at `root`, opened Mongo-free with the slice's filed answers (its gaps answered as the cut's were). */
  def recordingAt(root: Path): RecordingOf = slice => {
    val opened = IdentitySlice.openRecording(IdentitySlice.Recording(root, slice.country), slice.filed)
    new SliceHttp.Noting(opened.fetch, () => opened.leaf.metOnThread)
  }

  /** `dir`'s answers topped up from its recording until it decides alone lacking nothing (at most `rounds` rounds): the
   *  requests still lacking, none when it closed. */
  def repin(dir: Path, recordingOf: RecordingOf, rounds: Int = 4): Seq[String] = {
    var lacking = Seq.empty[String]
    var round   = 0
    var closed  = false
    while (!closed && round < rounds) {
      round += 1
      val slice   = IdentitySlice.readSlice(dir)
      val (_, _, widened) = IdentitySlice.decidePinned(slice, Some(recordingOf(slice)))
      val added   = widened.widened
      if (added.nonEmpty) SliceHttp.write(dir.resolve("http.jsonl.gz"), slice.http ++ added)
      lacking = rendered(IdentitySlice.decidePinned(IdentitySlice.readSlice(dir))._3.lacking.asScala)
      closed = lacking.isEmpty || added.isEmpty
    }
    lacking
  }

  /** The run failed: thrown, never `sys.exit` (the kept sbt server is the JVM). */
  final case class Failed(why: String) extends RuntimeException(s"IdentitySliceCoverage: $why") with scala.util.control.NoStackTrace

  final case class Options(slices: Seq[Path] = Nil, repin: Boolean = false, recordings: Option[Path] = None, needs: Option[Path] = None,
                           reportTo: Option[Path] = None)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                            => o
    case "--slices" :: v :: rest        => parse(rest, o.copy(slices = o.slices :+ Path.of(v)))
    case "--repin" :: rest              => parse(rest, o.copy(repin = true))
    case "--recordings" :: v :: rest    => parse(rest, o.copy(recordings = Some(Path.of(v))))
    case "--needs" :: v :: rest         => parse(rest, o.copy(needs = Some(Path.of(v))))
    case "--report" :: v :: rest        => parse(rest, o.copy(reportTo = Some(Path.of(v))))
    case other                          => throw Failed(s"unknown argument ${other.head}")
  }

  /** The report: each lacking slice and what it lacks (`perSlice` lines of it), the slices that could not be decided. */
  def report(found: Seq[Coverage], perSlice: Int = 12): Seq[String] =
    found.filterNot(_.covered).flatMap { c =>
      val head = c.failed.fold(s"  ${c.dir} (recording ${recordingKey(c.recording).getOrElse(c.recording)}): lacks ${c.lacking.size} answer(s)")(why => s"  ${c.dir}: $why")
      head +: (c.lacking.take(perSlice).map(l => s"      $l") ++
        Option.when(c.lacking.size > perSlice)(s"      ... ${c.lacking.size - perSlice} more"))
    }

  def main(args: Array[String]): Unit = {
    val o       = parse(args.toList)
    // every line said also kept in --report (the wrapper shows it apart from sbt's own lines)
    o.reportTo.foreach(f => Files.writeString(f, ""))
    def println(line: String): Unit = {
      Console.println(line)
      o.reportTo.foreach(f => Files.writeString(f, line + "\n", java.nio.file.StandardOpenOption.APPEND))
    }
    val started = tools.Stopwatch.start()
    val dirs    = slicesUnder(if (o.slices.nonEmpty) o.slices else Roots)
    if (dirs.isEmpty) throw Failed("no slice found")
    val found   = coverage(dirs)
    val missing = found.filterNot(_.covered)
    println(f"identity-slice-coverage: ${dirs.size} slice(s) replayed in ${started.seconds}%.1fs; ${missing.size} lack answers or could not be decided " +
      s"(${missing.map(_.lacking.size).sum} request(s) lacking)")
    report(found).foreach(println)
    o.needs.foreach { file =>
      val lines = missing.filter(_.failed.isEmpty).flatMap(c => recordingKey(c.recording).map(k => s"$k\t${c.country}")).distinct.sorted
      Files.writeString(file, lines.map(_ + "\n").mkString)
    }
    if (missing.isEmpty) return
    if (!o.repin) {
      println("SLICE ANSWERS MISSING: the slices above ask requests their fixtures do not hold. Re-pin them all from their recordings: " +
        "scripts/identity-slice-coverage --repin")
      throw Failed(s"${missing.size} slice(s) lack answers")
    }
    val recordings = o.recordings.getOrElse(throw Failed("--repin reads the recordings from --recordings DIR (the shared cache's recordings/)"))
    val left = missing.flatMap { c =>
      val recording = recordingKey(c.recording).map(k => recordings.resolve(k).resolve("pair/test/resources/fixtures"))
      if (c.failed.nonEmpty) Some(s"${c.dir}: not re-pinned, ${c.failed.get}")
      else recording.filter(r => Files.isDirectory(r.resolve("corpus"))) match {
        case None => Some(s"${c.dir}: not re-pinned, its recording ${c.recording} is not under $recordings (fetch it: " +
          s"scripts/identity-fetch-recording.sh ${recordingKey(c.recording).getOrElse("<run>")} <dir> ${c.country})")
        case Some(r) =>
          val still = repin(c.dir, recordingAt(r))
          println(s"  ${c.dir}: re-pinned from ${recordingKey(c.recording).get}, ${c.lacking.size} request(s) were lacking" +
            (if (still.isEmpty) "" else s"; STILL lacks ${still.size}: ${still.take(5).mkString("; ")}"))
          Option.when(still.nonEmpty)(s"${c.dir}: still lacks ${still.size} request(s) after re-pinning")
      }
    }
    if (left.nonEmpty) { left.foreach(l => println(s"NOT RE-PINNED: $l")); throw Failed(s"${left.size} slice(s) not re-pinned") }
    println(s"identity-slice-coverage: ${missing.size} slice(s) re-pinned; commit their http.jsonl.gz")
  }
}
