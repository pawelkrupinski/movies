package clients.tools

import tools.Env

/**
 * The directory the recorded fixture trees live under — `test/resources/fixtures` relative
 * to the repository root by default. A process whose working directory is NOT the
 * repository root (the local stack's forked worker) or that keeps its trees elsewhere
 * (`scripts/hard-clusters.sh`) hands a different one to the fetches that replay and record.
 *
 * A value rather than a variable every `FakeHttpFetch` reads for itself: only the code
 * that starts a run — a `main`, or a suite that is its own root — resolves it from the
 * process, through [[fromEnv]].
 */
final case class FixtureRoot(directory: String) {

  /** The tree `fixtureDirectory` under this root. */
  def of(fixtureDirectory: String): String = s"$directory/$fixtureDirectory"
}

object FixtureRoot {

  /** `test/resources/fixtures`, relative to the working directory (the repository root for
   *  `sbt test` / `runMain`). */
  val RepositoryRelative: FixtureRoot = FixtureRoot("test/resources/fixtures")

  /** `KINOWO_FIXTURE_ROOT` when `env` names one, else [[RepositoryRelative]]. */
  def fromEnv(env: Env): FixtureRoot =
    env.get("KINOWO_FIXTURE_ROOT").filter(_.nonEmpty).map(FixtureRoot(_)).getOrElse(RepositoryRelative)
}
