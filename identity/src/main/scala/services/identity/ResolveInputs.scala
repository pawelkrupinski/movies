package services.identity

import services.movies.TitleNormalizer

/**
 * What a resolve of production's listings reads besides the listings and their lookups: the curation's pins, the
 * calibration and the title decorations. ONE value, built by [[ResolveInputs.of]] wherever production's identity is
 * resolved: the worker's model (`WorkerWiring.identityModel`), the drift checks (`scripts.IdentityDrift`,
 * `scripts.IdentityLiveDrift`) and, by name, `scripts.IdentitySlice` (`OlderRefs.resolveInputs`: its files compile into
 * refs older than this). A new input is a new parameter of [[of]], so every site that builds the inputs stops compiling
 * until it says where its value comes from. The live drift check passed no pins for weeks behind a default.
 */
final case class ResolveInputs(pins: PinConstraints, calibration: IdentityCalibration, decorations: TitleDecorations) {

  /** The incremental model over these inputs, its families kept in `store` under `rules`. */
  def incremental(lookups: IdentityLookups, normalizer: TitleNormalizer, store: IdentityModelStore, rules: String,
                  traces: IdentityTraceSink): IncrementalResolver =
    new IncrementalResolver(lookups, normalizer, calibration, pins, decorations, store = store, rules = rules, traces = traces)

  /** The rules version a model's stored families are kept under: the code's, and these inputs'. */
  def rulesVersion(code: String): String = IncrementalResolver.rulesVersion(code, calibration, decorations, pins)
}

object ResolveInputs {

  /** Production's inputs: the operator `pins` (`identity_pins`), the resolver's calibration and its title decorations. */
  def of(pins: Seq[Pin]): ResolveInputs = ResolveInputs(PinConstraints(pins), IdentityCalibration.resolver, TitleDecorations.resolver)

  /** No pins: a spec's corpus, or a tool that reads no curation and says so. */
  val Unpinned: ResolveInputs = of(Nil)
}
