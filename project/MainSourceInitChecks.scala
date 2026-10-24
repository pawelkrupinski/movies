import sbt._
import sbt.Keys._

/**
 * `-Wsafe-init` (the initialization checker: a field read before its initializer ran, `this` leaked out of a
 * constructor) for every module's MAIN sources only, never its test or test-support configurations.
 *
 * WHY NOT THE REST. Measured 2026-10-23 (Scala 3.9.0), the checker raised on main sources two warnings (one
 * declaration-order fix, one silenced cycle) and on everything else only false positives:
 *  - Every ScalaTest spec registers its cases from its constructor (`"x" should "y" in { ... }`), handing the half-built
 *    spec to ScalaTest's DSL: one warning per spec class (111), and on web's test sources a compiler crash (a
 *    `ClassCastException` in `transform.init.Semantic`) that no silencing `-Wconf` can stop.
 *  - worker's `fixtures` (the replay wirings: `TestWiring`, `FixtureTestWiring`, `ArchiveReplayWiring`) subclass the
 *    composition root, and the checker cannot promote the callbacks `WorkerWiring` hands its collaborators while it is
 *    built (an event-bus subscriber, `onTaskFinished` hooks, a filing callback): 5 per wiring class, none of them a read
 *    of an uninitialized field.
 * Every configuration starts from the options of the one it extends, so the flag goes on `Compile` and comes off in
 * `Test` (`it` and `page` extend it) and in `fixtures` (which extends `Compile`).
 *
 * The checker reads other files' compiled TASTy, which an incremental compile can still hold from before a change:
 * project/StaleTastyRetry.scala recovers a batch run from that.
 */
object MainSourceInitChecks extends AutoPlugin {
  // worker's `Fixtures` (build.sbt) by its name: a setting scoped to it is inert in a module without one.
  private val Fixtures = config("fixtures")
  override def trigger = allRequirements
  override def projectSettings: Seq[Setting[?]] = {
    val flag = "-Wsafe-init"
    Seq(Compile / scalacOptions += flag, Test / scalacOptions -= flag, Fixtures / scalacOptions -= flag)
  }
}
