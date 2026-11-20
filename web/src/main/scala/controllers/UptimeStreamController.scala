package controllers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import play.api.libs.json.Json
import play.api.mvc.*
import services.UptimeHistory
import services.UptimeMonitor.BucketListener

/** `GET /uptime/stream`: the /uptime page's live feed, each changed bucket as the page labels it. */
class UptimeStreamController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeHistory, bars: UptimeBars)
                            (using mat: Materializer) extends AbstractController(cc) {

  // SSE batching: a poll cycle can flip one bucket per active service at once.
  // Buffer generously (drop oldest under a burst), then coalesce everything that
  // lands inside BatchWindow into one frame so the page never sees a per-service
  // message storm. BatchMaxSize caps a single frame; BatchWindow bounds latency.
  private val StreamBufferSize = 2048
  private val BatchMaxSize     = 1024
  private val BatchWindow      = 250.millis

  def stream: Action[AnyContent] = adminAction {
    Ok.chunked(eventSource()).as("text/event-stream")
  }

  /** The live `/uptime/stream` feed. One listener per connected browser pushes
   *  each changed bucket as a `BarData`; `groupedWithin` then coalesces a burst
   *  into a SINGLE `data: [...]` SSE frame. A worker poll that flips N buckets
   *  at once therefore costs the page one frame and one DOM pass, not N — so the
   *  stream's cost stays flat as the cinema roster (and thus N) grows, instead
   *  of one frame per scraper per poll. */
  private[controllers] def eventSource(): Source[String, org.apache.pekko.NotUsed] = {
    val (queue, source) = Source.queue[BarData](StreamBufferSize, OverflowStrategy.dropHead)
      .preMaterialize()

    val listener: BucketListener = { (service, snapshot) =>
      queue.offer(bars.bar(service, snapshot))
      ()
    }
    monitor.addListener(listener)

    source
      .groupedWithin(BatchMaxSize, BatchWindow)
      .map(batch => s"data: ${Json.toJson(batch)}\n\n")
      .watchTermination() { (_, done) =>
        done.onComplete(_ => monitor.removeListener(listener))(using ExecutionContext.global)
        org.apache.pekko.NotUsed
      }
  }
}
