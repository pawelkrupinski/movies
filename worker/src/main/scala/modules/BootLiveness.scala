package modules

/**
 * The liveness signal `/health` reports, across a boot. Starts permissive — `/health`
 * comes up BEFORE wiring (a slow Mongo boot must not fail the check), and a
 * still-booting process IS alive — then is handed the real probe (the fleet of
 * LivenessWatchdogs) once wiring is up. Owned by `WorkerMain.main`, which builds one
 * and hands it to the health endpoint.
 */
private[modules] final class BootLiveness {
  @volatile private var probe: () => Boolean = () => true

  def isAlive: Boolean = probe()

  /** From now on, report whatever `real` says. */
  def becomes(real: () => Boolean): Unit = probe = real
}
