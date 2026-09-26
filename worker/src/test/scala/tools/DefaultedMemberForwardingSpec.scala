package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{MongoCachingDetailFetch, UptimeMonitor}
import services.metrics.{MeteredTaskQueue, WorkerTaskMetrics}
import services.tasks.{CachingTaskQueue, TaskQueue}

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A decorator must forward every member its trait gives a DEFAULT body, or say why not.
 *
 * Inheriting the default compiles, runs and answers something plausible — which is how
 * `FallbackHttpFetch` dropped the auth headers of `get(url, headers)` (the default calls
 * `get(url)`), and how a wrapper that forgets `getBytes` round-trips a legacy
 * single-byte page through UTF-8. `DecoratorTransparencySpec` holds the cinema-scraper
 * decorators to this; this holds every other decorated trait: [[HttpFetch]] and
 * [[TaskQueue]] (whose default `watchWaiting` answers "no change stream", silently
 * turning an event-driven worker back into a poller).
 *
 * Decorators and defaulted members are both found by reflection ([[TraitDecorators]]),
 * so a new wrapper or a new default is covered — or fails here until it is.
 */
class DefaultedMemberForwardingSpec extends AnyFlatSpec with Matchers {

  /** A trait, how to build each of its decorators, and the defaults a decorator may
   *  keep — each with the reason it is safe. */
  private final case class Decorated[T](
    `trait`:    Class[T],
    anchors:    Seq[Class[?]],
    factories:  Map[Class[?], T => T],
    mayInherit: Map[(Class[?], String), String]
  )

  private val getAsync = "getAsync(String)"
  private val headerGet = "get(String, Map)"

  private val httpFetch = Decorated[HttpFetch](
    classOf[HttpFetch],
    anchors = Seq(classOf[HttpFetch], classOf[FallbackHttpFetch]),
    factories = Map(
      classOf[RateLimitedHttpFetch]        -> ((d: HttpFetch) => new RateLimitedHttpFetch(d, intervalFor = _ => None)),
      classOf[ThrottledHttpFetch]          -> ((d: HttpFetch) => new ThrottledHttpFetch(d)),
      classOf[HostCircuitBreakerHttpFetch] -> ((d: HttpFetch) => new HostCircuitBreakerHttpFetch(d)),
      classOf[MemoizedHttpFetch]           -> ((d: HttpFetch) => new MemoizedHttpFetch(d)),
      classOf[CachingDetailFetch]          -> ((d: HttpFetch) => new CachingDetailFetch(d)),
      classOf[MongoCachingDetailFetch]     -> ((d: HttpFetch) => new MongoCachingDetailFetch(d, None, 1.hour, "decorator_probe", new services.TtlIndexMismatches)),
      classOf[MonitoringHttpFetch]         -> ((d: HttpFetch) => new MonitoringHttpFetch(d, new UptimeMonitor())),
      classOf[CountingHttpFetch]           -> ((d: HttpFetch) => new CountingHttpFetch(d, HttpOutcomeRecorder.noop)),
      classOf[StickyShardHttpFetch]        -> ((d: HttpFetch) => new StickyShardHttpFetch(IndexedSeq(d))),
      classOf[FallbackHttpFetch]           -> ((d: HttpFetch) => new FallbackHttpFetch(Seq("only" -> d))),
      classOf[SessionWarmingHttpFetch]     -> ((d: HttpFetch) => new SessionWarmingHttpFetch(d, "https://decorator.test/")),
      classOf[services.observations.ObservingHttpFetch] -> ((d: HttpFetch) =>
        new services.observations.ObservingHttpFetch(d, services.observations.ObservationStore.inMemory(java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)))),
      // An empty store observed nothing, so every call reaches the live fetch it decorates.
      classOf[services.identity.ObservedFirstHttpFetch] -> ((d: HttpFetch) =>
        new services.identity.ObservedFirstHttpFetch(services.observations.ObservationStore.inMemory(java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)), d)),
      // A budget with room: every call reaches the fetch it decorates.
      classOf[services.identity.ShadowLiveFetch] -> ((d: HttpFetch) =>
        new services.identity.ShadowLiveFetch(d, new services.identity.ShadowLookupBudget(Int.MaxValue, 0.seconds, _ => ())))
    ),
    mayInherit =
      // The default runs `this.get(url)` on a pool thread — through the decorator's own
      // `get`, so the decoration still applies.
      Seq(classOf[RateLimitedHttpFetch], classOf[ThrottledHttpFetch], classOf[HostCircuitBreakerHttpFetch],
          classOf[MemoizedHttpFetch], classOf[CachingDetailFetch], classOf[MongoCachingDetailFetch],
          classOf[MonitoringHttpFetch], classOf[CountingHttpFetch], classOf[StickyShardHttpFetch],
          classOf[FallbackHttpFetch], classOf[SessionWarmingHttpFetch], classOf[services.observations.ObservingHttpFetch],
          classOf[services.identity.ObservedFirstHttpFetch], classOf[services.identity.ShadowLiveFetch])
        .map(c => (c: Class[?], getAsync) -> "the default async get goes through the decorator's own get").toMap ++
      Map(
        // A detail page does not vary by request header; both caches key on the URL alone.
        (classOf[CachingDetailFetch], headerGet)      -> "detail pages do not vary by header; keyed on the URL",
        (classOf[MongoCachingDetailFetch], headerGet) -> "detail pages do not vary by header; keyed on the URL"
      )
  )

  private val taskQueue = Decorated[TaskQueue](
    classOf[TaskQueue],
    anchors = Seq(classOf[TaskQueue], classOf[MeteredTaskQueue]),
    factories = Map(
      classOf[CachingTaskQueue] -> ((d: TaskQueue) => new CachingTaskQueue(d)),
      classOf[MeteredTaskQueue] -> ((d: TaskQueue) => new MeteredTaskQueue(d,
        new WorkerTaskMetrics("pl", new WorkerTaskMetrics.Series(poolSize = 1, countryCodes = Seq("pl")))))
    ),
    mayInherit = Map.empty
  )

  Seq(httpFetch, taskQueue).foreach { case Decorated(t, anchors, factories, mayInherit) =>
    val name = t.getSimpleName

    s"Every $name decorator" should "be listed here, so it is held to the check below" in {
      anchors.flatMap(TraitDecorators.discover(t, _)).toSet shouldBe factories.keySet
    }

    it should "have a default to forward at all (else this spec checks nothing)" in {
      TraitDecorators.defaulted(t) should not be empty
    }

    factories.toSeq.sortBy(_._1.getSimpleName).foreach { case (decoratorClass, decorate) =>
      it should s"forward every defaulted member through ${decoratorClass.getSimpleName}" in {
        val dropped = TraitDecorators.defaulted(t).flatMap { member =>
          val signature = TraitDecorators.signature(member)
          if (mayInherit.contains((decoratorClass, signature))) None
          else {
            // One answer per member, so "the decorator returned the delegate's answer" is
            // an identity, not a lookalike.
            val answers = scala.collection.mutable.Map.empty[String, Any]
            val (delegate, calls) = TraitDecorators.recording(t, m => answers.synchronized {
              answers.getOrElseUpdate(TraitDecorators.signature(m), TraitDecorators.sampleAnswer(m.getReturnType))
            })
            val args     = member.getParameterTypes.toSeq.map(TraitDecorators.sampleArgument)
            val answered = member.invoke(decorate(delegate), args.map(_.asInstanceOf[AnyRef])*)
            val reached  = calls.asScala.exists(c => c.method == signature && c.args == args)
            val same = (answered, answers.get(signature)) match {
              case (a: Array[Byte], Some(e: Array[Byte])) => a.sameElements(e)
              case (null, None)                           => true // a Unit member
              case (a, Some(e))                           => a == e
              case _                                      => false
            }
            if (reached && same) None
            else Some(s"$signature: delegate ${if (reached) "reached" else "NEVER reached with the same arguments"}, " +
              s"answer ${if (same) "forwarded" else "not the delegate's"} (delegate saw ${calls.asScala.map(_.method).mkString(", ")})")
          }
        }
        withClue(s"${decoratorClass.getSimpleName} inherits $name defaults instead of forwarding them — " +
          s"override each to call the delegate, or list it in `mayInherit` with the reason it is safe:\n") {
          dropped shouldBe empty
        }
      }
    }
  }
}
