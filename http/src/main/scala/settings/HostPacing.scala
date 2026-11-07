package settings

// The per-host pace's knob and value, beside the HostPolicies table (tools) that names a knob per paced host. The
// process configuration reads a knob's live value (`ProcessConfiguration.hostPace`) and the caller passes it in.
/** A paced host's `KINOWO_*_PACE_MS` knob — its identity is the knob, so one host's pace can
 *  never be read as another's. */
enum PaceKnob(val key: String) {
  case Filmstarts extends PaceKnob("KINOWO_FILMSTARTS_PACE_MS")
  case Sensacine  extends PaceKnob("KINOWO_SENSACINE_PACE_MS")
  case Flicks     extends PaceKnob("KINOWO_FLICKS_PACE_MS")
  case FlicksUs   extends PaceKnob("KINOWO_FLICKS_US_PACE_MS")
  case Alamo      extends PaceKnob("KINOWO_ALAMO_PACE_MS")
  case ShowcaseUs extends PaceKnob("KINOWO_SHOWCASE_US_PACE_MS")
  case Landmark   extends PaceKnob("KINOWO_LANDMARK_PACE_MS")
  case Ocine      extends PaceKnob("KINOWO_OCINE_PACE_MS")
  case Taquilla   extends PaceKnob("KINOWO_TAQUILLA_PACE_MS")
  case Kinoprogramm extends PaceKnob("KINOWO_KINOPROGRAMM_PACE_MS")
  case Kinoheld   extends PaceKnob("KINOWO_KINOHELD_PACE_MS")
  case Cineamo    extends PaceKnob("KINOWO_CINEAMO_PACE_MS")
  case WikidataFleet extends PaceKnob("KINOWO_WIKIDATA_FLEET_PACE_MS")
}
/** The live minimum gap between two requests to a paced host. */
final case class HostPace(value: java.time.Duration) extends AnyVal
