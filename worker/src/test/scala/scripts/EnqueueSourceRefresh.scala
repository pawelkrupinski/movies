package scripts

import services.MongoConnection
import services.refresh.RefreshableSources
import services.tasks.{EnqueueResult, MongoTaskQueue, TaskType}
import tools.CommandLine

/**
 * Asks a country's worker to fetch again what one stored external source holds for given keys
 * (`services.refresh.RefreshableSource`), and to kick what moved downstream: `RefreshSource` tasks on the country's
 * queue, at most `RefreshableSources.MaxKeys` keys each. Sources: `tmdb-films` (TMDB film ids: their records, then the
 * questions of the identity families they reach, which the model re-resolves), `imdb`, `metacritic`, `rt`, `filmweb`
 * (Poland only; each a rating source's rows by TMDB film id). The run's outcome is on the `/tasks` page ("lastResults",
 * `RefreshSource`) and in the worker log ("refresh <source> for N keys").
 *
 * Dry run by DEFAULT — pass `--apply` to enqueue. Against prod (Poland's database), through the vault and the tunnel:
 *
 *   MONGODB_DB=kinowo scripts/with-prod-mongo.sh scripts/sbt-warm "worker/Test/runMain scripts.EnqueueSourceRefresh tmdb-films 1525208 1517105 --apply"
 */
object EnqueueSourceRefresh {

  val commandLine: CommandLine = CommandLine("EnqueueSourceRefresh",
    "Enqueues RefreshSource tasks for one source and the keys given; a dry run unless --apply.",
    Seq(CommandLine.Flag("apply", "enqueue, rather than say what would be")),
    Seq(CommandLine.Positional("source", s"the source: ${RefreshableSources.Names.mkString(", ")}"),
        CommandLine.Positional("keys", "its keys (TMDB film ids)", repeatable = true)))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val source    = arguments.requiredPositional("source")
    val keys      = arguments.positionalsFrom("keys").distinct
    if (!RefreshableSources.Names.contains(source)) arguments.fail(s"no source '$source': ${RefreshableSources.Names.mkString(", ")}")
    if (keys.isEmpty) arguments.fail("name at least one key")
    val conn = MongoConnection.forProcess(_root_.settings.ProcessConfiguration.resolve(), required = services.MongoRequirement.Required)
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
