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

  /** The incremental model over these inputs, its families kept in `store` under the rules version of `code` and these
   *  very inputs ([[rulesVersion]]): built here, never handed in, so no caller can keep families under a version that
   *  leaves out an input the model reads. */
  def incremental(lookups: IdentityLookups, normalizer: TitleNormalizer, store: IdentityModelStore, code: String,
                  traces: IdentityTraceSink): IncrementalResolver =
    new IncrementalResolver(lookups, normalizer, calibration, pins, decorations, store = store, rules = rulesVersion(code), traces = traces)

  /** The rules version a model's stored families are kept under: the code's, and these inputs'. */
  def rulesVersion(code: String): String = IncrementalResolver.rulesVersion(code, calibration, decorations, pins)
}

object ResolveInputs {

  /** Production's inputs: the operator `pins` (`identity_pins`), the hand judgements of takes (`judged`: the worker's
   *  bundled ones, [[TakeJudgements.Index.bundled]]), the resolver's calibration and its title decorations. */
  def of(pins: Seq[Pin], judged: TakeJudgements.Index = TakeJudgements.Index.Empty): ResolveInputs =
    ResolveInputs(PinConstraints(pins, judged), IdentityCalibration.resolver, TitleDecorations.resolver)

  /** No pins: a spec's corpus, or a tool that reads no curation and says so. */
  val Unpinned: ResolveInputs = of(Nil)
}
