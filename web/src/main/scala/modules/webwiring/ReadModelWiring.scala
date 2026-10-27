package modules.webwiring

import controllers.MovieControllerService
import modules.Wiring
import services.readmodel.{MongoReadModelRepository, ReadModelReader, WebReadModel}

/** ── Denormalised read model ──────────────────────────────────────────────────
 *  The serving app reads from the worker-maintained `web_movies` /
 *  `web_screenings` collections via `WebReadModel`, kept warm by their change
 *  streams. It deliberately does NOT watch `movies` — a showtime edit there
 *  now reaches the web as one small screening-document delta, not a full-record
 *  re-push — and never reads the source corpus at all. */
trait ReadModelWiring { self: Wiring =>

  lazy val readModelRepository: ReadModelReader = new MongoReadModelRepository(mongoConnection.database, decodeFailures = webDecodeFailureMetrics, stopwatch = stopwatch)
  lazy val webReadModel: WebReadModel = managedResources.stopping(new WebReadModel(readModelRepository,
    reloadInterval    = processConfiguration.readModelReloadInterval(WebReadModel.DefaultReloadInterval),
    coldRetryInterval = processConfiguration.readModelColdRetryInterval(WebReadModel.DefaultColdRetryInterval),
    streamMetrics     = webReadModelStreamMetrics,
    clock             = clock,
    servingCountry    = Some(country),
    ticks             = ticks))

  // Read at scrape time, so the model (forced at boot) is never built by the metric.
  lazy val webReadModelStreamMetrics = new services.metrics.WebReadModelStreamMetrics(webJvmMetrics.registry, country.code,
    collection => webReadModel.streamLive(collection))

  // Reads come straight from the read model; enrichment + projection happen in
  // the worker process.
  lazy val movieControllerService = new MovieControllerService(webReadModel, clock)
}
