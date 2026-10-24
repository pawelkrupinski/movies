import sbt._
import sbt.Keys._

/**
 * A batch sbt run (CI's rows, the pre-push hook) whose compile failed on a STALE TASTy file drops that configuration's
 * classes and runs its commands once more.
 *
 * WHY IT HAPPENS. `-Wsafe-init` (project/MainSourceInitChecks.scala) follows a constructor into the methods it calls,
 * and reads the bodies of those in other files from their compiled TASTy. Zinc compiles in cycles: the changed sources
 * first, their dependents after, once it has seen the API change. So when the first cycle holds a class whose
 * initialization reaches an UNCHANGED dependent of a changed signature, the checker reads that dependent's TASTy from
 * before the change, and the compile fails ("wrong number of arguments at pickleQuotes ... expected: 4, found: 5",
 * "Ignoring analyses of tmdbClientOver due to error in reading TASTy") before zinc's next cycle would have recompiled
 * it. Main #1397 (run 2428): 2cda2bf57 dropped a TmdbClient constructor parameter, the first cycle held a wiring class
 * mixing in HttpWiring, and HttpWiring's `new TmdbClient(...)` was read as compiled before. No build cache is needed:
 * the same change over a warm local compile, plus any edit to WorkerWiring.scala, fails the same way, and a clean
 * compile (one cycle) passes. No compiler flag stops the checker reading TASTy (Scala 3.9.0).
 *
 * THE RECOVERY. A missing product makes zinc recompile its source, so with the classes gone the whole configuration
 * compiles in one cycle and nothing stale is left to read. Once per sbt JVM, only for a configuration whose compile
 * logged the signature during this run, and only in a batch run: in sbt's shell (sbt-warm's server) the failure is
 * printed as it was, and running `staleTastyDrop` there, then the command again, recovers by hand.
 */
object StaleTastyRetry extends AutoPlugin {
  override def trigger = allRequirements

  private val Signature = "due to error in reading TASTy"
  private val Arm = "staleTastyArm"
  private val Retry = "staleTastyRetry"
  private val Drop = "staleTastyDrop"
  /** A JVM property rather than a field: a `reload` loads this object afresh, the property survives it. */
  private val Retried = "kinowo.staleTastyRetried"
  private val Batch = AttributeKey[(Long, List[Exec])]("staleTastyBatch", "When the batch started, and its commands.")

  /** What sbt queues ahead of a batch's own commands while it loads; the first of them restores the failure handler
   *  sbt had before the load, so the handler is set by a command queued after them (`Arm`). */
  private val SbtBootCommands = Set("sbtPopOnFailure", "resumeFromFailure", "notifyUsersAboutShell", "iflast shell")

  override def globalSettings: Seq[Setting[_]] = Seq(
    commands ++= Seq(Command.command(Arm)(setHandler), Command.command(Retry)(retry), Command.command(Drop)(drop)),
    onLoad := onLoad.value.andThen(arm)
  )

  private def arm(s: State): State = {
    val (boot, batch) = s.remainingCommands.span(e => SbtBootCommands(e.commandLine))
    val interactive = batch.exists(e => e.commandLine == "shell" || e.commandLine.startsWith("~"))
    if (batch.isEmpty || interactive || sys.props.contains(Retried)) s
    else s.put(Batch, (System.currentTimeMillis, batch)).copy(remainingCommands = boot ++ (Exec(Arm, None) :: batch))
  }

  private def setHandler(s: State): State =
    if (s.onFailure.isDefined) s.remove(Batch) else s.copy(onFailure = Some(Exec(Retry, None)))

  /** sbt runs this after a failed command, with every command after the failure dropped and no handler left. */
  private def retry(s: State): State =
    s.get(Batch) match {
      case Some((since, batch)) =>
        val stale = staleClassDirectories(s, since)
        if (stale.isEmpty) s.remove(Batch).fail
        else {
          sys.props(Retried) = "true"
          dropAll(s, stale)
          s.log.warn(s"[stale TASTy] running the batch again: ${batch.map(_.commandLine).mkString(" ")}")
          s.remove(Batch).copy(remainingCommands = batch)
        }
      case None => s.fail
    }

  private def drop(s: State): State = {
    val stale = staleClassDirectories(s, since = 0L)
    if (stale.isEmpty) s.log.info("[stale TASTy] no compile logged a stale TASTy read")
    dropAll(s, stale)
    s
  }

  private def dropAll(s: State, dirs: Seq[File]): Unit = dirs.foreach { dir =>
    s.log.warn(s"[stale TASTy] a compile read an out-of-date TASTy file: dropping $dir so it compiles in one cycle")
    IO.delete(dir)
  }

  /** The class directory of every configuration whose compile log, written since `since`, holds the signature. */
  private def staleClassDirectories(s: State, since: Long): Seq[File] = {
    val extracted = Project.extract(s)
    for {
      ref     <- extracted.structure.allProjectRefs
      config  <- extracted.getOpt(ref / ivyConfigurations).getOrElse(Nil)
      classes <- extracted.getOpt(ref / config / classDirectory).toSeq
      logs = Seq("compile", "compileIncremental").map { task =>
        extracted.get(ref / target) / "streams" / config.name / task / "_global" / "streams" / "out"
      }
      if logs.exists(log => log.isFile && log.lastModified >= since && IO.read(log).contains(Signature))
    } yield classes
  }
}
