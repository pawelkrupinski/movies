package scripts

import services.MongoConnection
import services.tasks.RefreshableSources
import services.tasks.{EnqueueResult, MongoTaskQueue}
import services.tasktypes.TaskType
import tools.CommandLine

/**
 * Asks a country's worker to fetch again what one stored external source holds for given keys
 * (`services.tasks.RefreshableSource`), and to kick what moved downstream: `RefreshSource` tasks on the country's
 * queue, at most `RefreshableSources.MaxKeys` keys each. Sources: `tmdb-films` (TMDB film ids: their records, then the
 * questions of the identity families they reach, which the model re-resolves), `imdb`, `metacritic`, `rt`, `filmweb`
 * (Poland only; each a rating source's rows by TMDB film id). The run's outcome is on the `/tasks` page ("lastResults",
 * `RefreshSource`) and in the worker log ("refresh <source> for N keys").
 *
 * Dry run by DEFAULT — pass `--apply` to enqueue. Run it through `scripts/source-refresh <database> <source> <keys...>
 * [--apply]` (the vault, the tunnel, and a sbt server started for the call): it refuses to run unless the environment it
 * reads carries the call's own run id (`--run`, [[refusal]]), so a kept sbt server started for another call — another
 * database's environment — can never enqueue.
 */
object EnqueueSourceRefresh {

  val commandLine: CommandLine = CommandLine("EnqueueSourceRefresh",
    "Enqueues RefreshSource tasks for one source and the keys given; a dry run unless --apply.",
    Seq(CommandLine.Flag("apply", "enqueue, rather than say what would be"),
        CommandLine.Value("run", "id", "the call's run id, as scripts/source-refresh exports it in KINOWO_SOURCE_REFRESH_RUN")),
    Seq(CommandLine.Positional("source", s"the source: ${RefreshableSources.Names.mkString(", ")}"),
        CommandLine.Positional("keys", "its keys (TMDB film ids)", repeatable = true)))

  /** Why this process must not run for the call that named `run`: the run id its environment carries (`seen`,
   *  KINOWO_SOURCE_REFRESH_RUN) is not that call's — a kept sbt server's, another call's database in it. None when it is. */
  def refusal(seen: Option[String], run: Option[String]): Option[String] = (run, seen) match {
    case (None, _)                                    => Some("no --run: start it with scripts/source-refresh")
    case (Some(asked), Some(seen)) if asked == seen   => None
    case (Some(asked), seen)                          =>
      Some(s"this JVM's environment was not started for run $asked (it carries ${seen.getOrElse("none")}): a kept sbt server " +
        "from another call, whose database is not this call's; start it with scripts/source-refresh")
  }

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val configuration = _root_.settings.ProcessConfiguration.resolve()
    refusal(configuration.sourceRefreshRun.map(_.value), arguments.get("run")).foreach { why => println(s"REFUSED: $why"); sys.exit(2) }
    val source    = arguments.requiredPositional("source")
    val keys      = arguments.positionalsFrom("keys").distinct
    if (!RefreshableSources.Names.contains(source)) arguments.fail(s"no source '$source': ${RefreshableSources.Names.mkString(", ")}")
    if (keys.isEmpty) arguments.fail("name at least one key")
    val conn = new MongoConnection(configuration.toolMongoConnection, services.MongoRequirement.Required)
    val db   = conn.database.getOrElse { println("Could not open the database — is MONGODB_URI set?"); sys.exit(1) }
    try {
      val batches = keys.grouped(RefreshableSources.MaxKeys).toSeq
      println(s"database ${db.name}: refresh $source for ${keys.size} keys in ${batches.size} task(s): ${keys.mkString(" ")}")
      if (!arguments.flag("apply")) println("dry run: nothing enqueued (pass --apply)")
      else {
        val clock  = java.time.Clock.systemUTC()
        val queue  = new MongoTaskQueue(Some(db))
        val failed = batches.count { batch =>
          queue.enqueue(TaskType.RefreshSource, RefreshableSources.dedupKey(source, batch), RefreshableSources.payload(source, batch),
            submittedAt = clock.instant()) match {
            case EnqueueResult.Added          => println(s"enqueued ${batch.size} keys"); false
            case EnqueueResult.Duplicate      => println(s"already queued: ${batch.size} keys' task is waiting or running"); false
            case EnqueueResult.Failed(reason) => println(s"NOT enqueued: $reason"); true
          }
        }
        if (failed > 0) sys.exit(1)
      }
    } finally conn.close()
  }
}
