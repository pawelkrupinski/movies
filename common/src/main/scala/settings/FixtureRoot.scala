package settings

import java.nio.file.Path

/**
 * `KINOWO_FIXTURE_ROOT` — the directory the recorded fixture trees live under,
 * `test/resources/fixtures` relative to the repository root by default. A process whose
 * working directory is NOT the repository root (the local stack's forked worker) or that
 * keeps its trees elsewhere (`scripts/hard-clusters.sh`) hands a different one to the fetches
 * that replay and record.
 */
final case class FixtureRoot(value: Path) extends AnyVal {

  /** The tree `fixtureDirectory` under this root. */
  def of(fixtureDirectory: String): String = value.resolve(fixtureDirectory).toString
}

object FixtureRoot {

  /** `test/resources/fixtures`, relative to the working directory (the repository root for
   *  `sbt test` / `runMain`). */
  val RepositoryRelative: FixtureRoot = FixtureRoot(Path.of("test/resources/fixtures"))
}
