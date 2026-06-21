package services.config

import tools.Env

/** A config knob as discovered + published by one process: its key, type,
 *  compiled-in default, and the value that process is currently using
 *  (post-override). One row per (app, key) in the `env_registry` collection. */
final case class RegisteredKnob(
  app:     String,
  key:     String,
  kind:    Env.Kind,
  default: Option[String],
  current: Option[String]
)

/** What one process reports for a key on the admin page. */
final case class AppValue(app: String, current: Option[String])

/** A merged admin-page row: one key across all reporting apps, plus the active
 *  override. `default`/`kind` come from whichever app registered the knob (they
 *  agree — same compiled-in call). `overrideValue` is None when no flip is set,
 *  so the page shows the "Set / Override" affordance instead of a value. */
final case class EnvConfigRow(
  key:           String,
  kind:          Env.Kind,
  default:       Option[String],
  apps:          Seq[AppValue],
  overrideValue: Option[String]
)

/** Decides which keys are SAFE to surface on the admin page. Auto-registration
 *  captures every key read through [[Env]] — including secrets (API tokens,
 *  Mongo URIs, OAuth creds, proxy passwords). Those must never be displayed or
 *  flippable, so a key whose name matches any secret marker is excluded from
 *  the registry publish, the admin rows, AND set/reset. Numeric tuning knobs
 *  (`positiveInt`/`positiveLong`, all `KINOWO_*`) never match, so the page keeps
 *  them plus any non-secret string flag. */
object EnvKnobClassifier {
  // Substrings (case-insensitive) that mark a key as a secret/credential or as
  // structural infra that can't be re-pointed live. Errs toward hiding.
  private val SecretMarkers = Seq(
    "TOKEN", "SECRET", "PASSWORD", "PASS", "KEY", "CRED", "COOKIE",
    "AUTH", "URI", "URL", "MONGO", "CLIENT", "ALLOWLIST", "PRIVATE", "PROXY"
  )

  def isSecret(key: String): Boolean = {
    val upper = key.toUpperCase
    SecretMarkers.exists(upper.contains)
  }
}
