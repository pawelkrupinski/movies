package tools

import scala.annotation.nowarn

/**
 * Identity types reached under the name the ref gives them. scripts/identity-slice, scripts/title-trace and
 * scripts/identity-what-if drop this checkout's tool files (their TOOL_FILES, this one among them) into every ref they
 * decide under, so a tool file naming a type by its newer name fails its compile in a ref from before the rename; named
 * through this object, it compiles in either. Both packages are imported in a scope that is `@nowarn("msg=unused
 * import")`: the one a ref does not keep the type in is unused there.
 */
@nowarn("msg=unused import")
object OlderRefTypes {
  import services.identity.*
  import services.identity.measures.*
  import services.identity.vocabulary.*
  // IdentityMeasures, in whichever package the ref keeps it: services.identity before 2026-11-20, services.identity.measures since
  import IdentityMeasures.*

  /** TMDB's record of a candidate film: `IdentityMeasures.Film` before 2026-11-20, then `services.identity.Film`, and
   *  `services.identity.measures.Film` since the measures' package (2026-11-20, lane P6). */
  type TmdbFilm = Film

  /** The record of a film known by its title alone. */
  def tmdbFilm(title: String): TmdbFilm = Film(title)

  /** An operator's pin: `services.identity.Pin` before 2026-11-20, `services.identity.vocabulary.Pin` since. */
  type OperatorPin = Pin
}
