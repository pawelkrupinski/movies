package tools

import settings.ProcessConfiguration

/**
 * The configuration a suite — or a tool's `main` — runs under, resolved ONCE per instance.
 *
 * A spec is the root of its own run, the way `AppLoader` is the web tier's: it resolves one
 * [[ProcessConfiguration]] and reads typed values off it (the Mongo target, the TMDB key, the
 * fixture root), never the process itself — `ProcessAccessLintSpec` holds every spec, page
 * test and tool main to that. A spec that needs a value the process does not decide builds
 * the typed value directly, or a `ProcessConfiguration` over `Env.of(...)`.
 *
 * Lazy, so a suite that never asks resolves nothing; per instance, so no suite sees another's.
 */
trait SuiteConfiguration {
  protected lazy val configuration: ProcessConfiguration = ProcessConfiguration.resolve()
}
