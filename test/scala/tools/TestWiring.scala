package tools

import modules.Wiring
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.Stoppable

trait TestWiring extends Wiring {
  override val controllerComponents: ControllerComponents = stubControllerComponents()

  override def environmentMode: Mode = Mode.Test

  def quiesce(stoppables: Stoppable*): Unit =
    stoppables.foreach(_.stop())

  /** Drain the event-cascade worker pools so every `TmdbResolved` /
   *  `ImdbIdMissing` / `ImdbIdResolved` published during the scrape is
   *  processed end to end (URL discovery + rating scrape for MC/RT/FW,
   *  IMDb GraphQL + suggestion lookup, TMDB search + external_ids /
   *  credits / details). Uses the same `cascadeDrainOrder` production
   *  shutdown does — single source of truth for the producer→consumer
   *  ordering. */
  def drainServices(): Unit = quiesce(cascadeDrainOrder: _*)
}
