package services.movies


import models.MovieRecord
import services.freshness.{FreshnessKind, FreshnessStore}
import services.tasks.{QueueEnrichmentRetrigger, RatingTasks, RetriggerTask}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

/** Why the next-day leg's unchanged film was rewritten: which enrichment's answer moved between the days, and which
 *  of its inputs changed on day one after its last ask with no ask following. Verify scrape recordings #23 ("Cienie
 *  zapomnianych przodków", "Plemię": the cinemas' directors reached the film in a projection that re-asked nothing)
 *  and #26 ("Afrykanska Przygoda": a detail page's director) each read only as "an unchanged film rewritten", and
 *  each took a recording replay to name the write path that skipped [[MergeRetrigger]]. Which enrichment reads which
 *  inputs is [[MergeRetrigger.changedInputs]]'s — production's own table — and what each writes, [[RetriggerKind.writes]]. */
object NextDayReask {

  /** The freshness stamp a kind's handler writes on each ask: a rating task's, none for the IMDb-id resolve. */
  private def stampOf(kind: RetriggerKind): Option[FreshnessKind] = QueueEnrichmentRetrigger.taskOf(kind) match {
    case RetriggerTask.Rating(_, freshness) => Some(freshness)
    case RetriggerTask.ResolveImdbId        => None
  }

  /** One day-one write that changed an enrichment input: its place in the day's sequence of writes and asks, each
   *  enrichment it should have re-asked with its inputs' before → after, and the slots the write changed. */
  final case class InputChange(sequence: Long, kinds: Map[RetriggerKind, Seq[String]], slots: Seq[String])

  /** What the harness saw of one film on day one: its input changes, and each stamped enrichment's last ask. */
  final case class DayOne(changes: Seq[InputChange], lastAsk: Map[RetriggerKind, Long])

  /** A line for each enrichment whose answer differs between `dayOne` and `dayTwo`, and one for the other fields that
   *  moved (no re-askable enrichment writes them). Empty when the records agree outside the slots. */
  def explain(dayOne: MovieRecord, dayTwo: MovieRecord, seen: DayOne): Seq[String] = {
    val enrichments = RetriggerKind.all.flatMap { kind =>
      val moved = kind.writes(dayOne).zip(kind.writes(dayTwo)).collect { case ((name, a), (_, b)) if a != b => s"$name $a → $b" }
      Option.when(moved.nonEmpty) {
        val stamped = stampOf(kind).nonEmpty
        val since   = seen.lastAsk.get(kind)
        val unasked = seen.changes.filter(c => c.kinds.contains(kind) && since.forall(c.sequence > _))
        val asked   = since.fold(if (stamped) "it was never asked on day one" else "its asks are not stamped")(s => s"its last day-one ask was #$s")
        val why =
          if (unasked.nonEmpty)
            s"$asked, and its inputs changed after it: " +
              unasked.map(c => s"#${c.sequence} ${c.kinds(kind).mkString(", ")}${slotsOf(c)}").mkString("; ") +
              s" — no re-ask followed: check MergeRetrigger's case for the path that made that write"
          else s"$asked, and no input of it changed after that: it answered otherwise on unchanged inputs"
        s"$kind moved (${moved.mkString(", ")}): $why"
      }
    }
    val written = RetriggerKind.all.flatMap(_.writes(dayOne).map(_._1)).toSet ++ Set("data", "tmdbAttempt")
    val others  = dayOne.productElementNames.zip(dayOne.productIterator.zip(dayTwo.productIterator))
      .collect { case (name, (a, b)) if a != b && !written(name) => name }.toSeq
    enrichments ++ Option.when(others.nonEmpty)(
      s"${others.mkString(", ")} moved: no re-askable enrichment writes ${if (others.sizeIs == 1) "it" else "them"} — the identity take (see day-one lag)")
  }

  private def slotsOf(change: InputChange) =
    if (change.slots.isEmpty) "" else s" (slot${if (change.slots.sizeIs == 1) "" else "s"} ${change.slots.mkString(", ")})"

  /** `name: before → after`, a collection as what it gained and lost. */
  private[movies] def describe(name: String, before: Any, after: Any): String = (before, after) match {
    case (a: Iterable[?], b: Iterable[?]) =>
      val (was, now) = (a.toSeq.map(String.valueOf), b.toSeq.map(String.valueOf))
      s"$name: ${((now.diff(was)).map(v => s"+'$v'") ++ was.diff(now).map(v => s"-'$v'")).mkString(" ")}"
    case (Some(a: Iterable[?]), Some(b: Iterable[?])) => describe(name, a, b)
    case (a, b) => s"$name: $a → $b"
  }

  /** The day-one recording the diagnosis reads: every film's input-changing writes (by `MovieCache.onResident`, which
   *  sees every writer's, the projection's included) and every enrichment's ask (its handler's freshness stamp), on
   *  one sequence. The harness's clock stands still through day one, so the order is the sequence, not the instant. */
  final class Recorder(using LatestTitleYear) {
    private val sequence = new AtomicLong()
    private val held     = new ConcurrentHashMap[FilmId, (CacheKey, MovieRecord)]()
    private val changes  = new ConcurrentHashMap[FilmId, java.util.concurrent.ConcurrentLinkedQueue[InputChange]]()
    private val keysOf   = new ConcurrentHashMap[FilmId, Set[(CacheKey, Option[Int])]]()
    private val asks     = new ConcurrentHashMap[String, java.lang.Long]()

    /** For `MovieCache.onResident`: quick, as it asks — one comparison per write, a rendering only when an input moved. */
    def resident(key: CacheKey, film: Option[StoredMovieRecord]): Unit = film.foreach { stored =>
      keysOf.merge(stored.id, Set(key -> stored.record.tmdbId), _ ++ _)
      Option(held.put(stored.id, key -> stored.record)).foreach { case (beforeKey, before) =>
        val inputs = MergeRetrigger.changedInputs(before, beforeKey, stored.record, key)
        if (inputs.nonEmpty) {
          val rendered = inputs.valuesIterator.flatten.toSeq.distinct
            .map(i => i -> describe(i.name, i.read(before, beforeKey), i.read(stored.record, key))).toMap
          val slots = stored.record.data.collect { case (source, slot) if !before.data.get(source).contains(slot) =>
            models.Source.cinemaOf(source).fold(source.toString)(_.displayName) }.toSeq.distinct.sorted
          changes.computeIfAbsent(stored.id, _ => new java.util.concurrent.ConcurrentLinkedQueue[InputChange]())
            .add(InputChange(sequence.incrementAndGet(), inputs.map { case (kind, in) => kind -> in.toSeq.map(rendered).sorted }, slots))
        }
      }
    }

    /** Wraps the wiring's freshness store so each stamp — a handler's finished ask — takes its place in the sequence. */
    def asking(store: FreshnessStore): FreshnessStore = new FreshnessStore {
      def lastFetchedAt(key: String): Option[Instant] = store.lastFetchedAt(key)
      def markFresh(key: String, kind: FreshnessKind, at: Instant): Unit = { store.markFresh(key, kind, at); asks.put(key, java.lang.Long.valueOf(sequence.incrementAndGet())); () }
      def invalidate(key: String): Unit = store.invalidate(key)
      override def whenReady(kind: FreshnessKind): Future[Unit] = store.whenReady(kind)
      override def retention: tools.StampedRows = store.retention
      override def close(): Unit = store.close()
    }

    /** Where the recording stands: what the diagnosis reads as "day one" ends here. */
    def mark(): Long = sequence.get()

    /** `id`'s day one, up to `until`: its input changes, and each stamped enrichment's last ask under any key it held. */
    def dayOne(id: FilmId, until: Long): DayOne = {
      val keys = keysOf.getOrDefault(id, Set.empty)
      val lastAsk = RetriggerKind.all.flatMap { kind =>
        stampOf(kind).flatMap { freshness =>
          keys.iterator.flatMap { case (key, tmdb) => Option(asks.get(RatingTasks.dedupKey(freshness, key, tmdb))).map(_.longValue) }
            .filter(_ <= until).maxOption.map(kind -> _)
        }
      }.toMap
      DayOne(Option(changes.get(id)).fold(Seq.empty[InputChange])(_.asScala.toSeq.filter(_.sequence <= until)), lastAsk)
    }
  }
}
