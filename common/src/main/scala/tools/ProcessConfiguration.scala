package tools

import models.Country
import services.MongoAddress

/**
 * THE one place production code reads this process's environment variables and system
 * properties (through `Env.fromProcess`, Env's binding to them) and turns them into the typed
 * values a composition root hands down: the serving country, the Mongo address, the commit,
 * the port, the executable search path — and the `Env` itself, which the wirings read their
 * tuning knobs through and the admin page overrides.
 *
 * A `main` (AppLoader, WorkerMain, a tool's `main`) calls [[ProcessConfiguration.resolve]]
 * once and passes the values on; nothing below it asks the process anything
 * (`ProcessAccessLintSpec` holds `src/main` to that). A spec builds a configuration over
 * `Env.of(...)` — or the values themselves — instead.
 *
 * Facts about the PROCESS rather than tuning knobs (the commit, the port, PATH) are read with
 * `currentValue`, which does not register them as knobs on the admin page.
 */
final class ProcessConfiguration(val env: Env) {

  /** The country a single-country process (the web tier) serves — `KINOWO_COUNTRY`. */
  def country: Country = Country.fromEnv(env)

  /** Where this process's Mongo is — `MONGODB_URI` / `MONGODB_DB`. */
  def mongoAddress: MongoAddress = MongoAddress.fromEnv(env)

  /** The commit the running build was made from (`COMMIT_SHA`, set by the image build), or
   *  `unknown`. */
  def commit: String = env.currentValue("COMMIT_SHA").getOrElse("unknown")

  /** `PORT`, else `default`. */
  def port(default: Int): Int = env.currentValue("PORT").flatMap(_.toIntOption).getOrElse(default)

  /** `APP_MODE` — an override of the mode Play derived for itself, when set. */
  def applicationMode: Option[String] = env.currentValue("APP_MODE")

  /** The directories `PATH` lists, in order — where a tool's binary (vips) is looked for. */
  def executableSearchPath: Seq[java.nio.file.Path] =
    env.currentValue("PATH").toSeq.flatMap(_.split(java.io.File.pathSeparator)).filter(_.nonEmpty).map(java.nio.file.Path.of(_))
}

object ProcessConfiguration {

  /** This process's configuration: environment variables → system properties → `localFile`
   *  (`.env.local` by default). Call once, from a `main`. */
  def resolve(localFile: java.io.File = new java.io.File(".env.local")): ProcessConfiguration =
    new ProcessConfiguration(Env.fromProcess(localFile))
}
