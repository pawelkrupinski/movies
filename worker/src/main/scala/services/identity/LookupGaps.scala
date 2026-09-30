package services.identity

import services.lookups.LookupQuery

import java.util.concurrent.atomic.AtomicLong
import scala.util.control.NoStackTrace

/** The questions one resolve found unanswered, counted by KIND — the method, host and path with
 *  ids and query stripped (`GET api.themoviedb.org/3/search/movie`, `DETAIL`) — so a tick says
 *  which service its Unknowns wait on.
 *
 *  A gap is recorded on the thread whose lookup met it, and counted per thread too: a lookup is
 *  `Unknown` when ITS thread met one, so lookups on a prefetch's threads read side by side instead
 *  of taking turns around one counter. */
final class LookupGaps extends TmdbIdentityLookups.Gaps {
  private val count    = new AtomicLong()
  private val kinds    = new java.util.concurrent.ConcurrentHashMap[String, AtomicLong]()
  private val onThread = ThreadLocal.withInitial[java.lang.Long](() => 0L)
  def record(query: LookupQuery): Unit = {
    count.incrementAndGet()
    onThread.set(onThread.get + 1)
    kinds.computeIfAbsent(LookupGaps.kindOf(query), _ => new AtomicLong()).incrementAndGet()
    ()
  }
  def answered[A](read: => A): Answer[A] = {
    val before = onThread.get
    scala.util.Try(read).toOption.filter(_ => onThread.get == before).fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
  }
  def total: Long = count.get()
  def byKind: Map[String, Long] = { import scala.jdk.CollectionConverters._; kinds.asScala.view.mapValues(_.get).toMap }
}

object LookupGaps {
  def kindOf(query: LookupQuery): String = query.key.split(' ') match {
    case Array("DETAIL", _*)    => "DETAIL"
    case Array(method, url, _*) =>
      val u = scala.util.Try(new java.net.URI(url)).toOption
      s"$method ${u.flatMap(x => Option(x.getHost)).getOrElse("")}${u.flatMap(x => Option(x.getPath)).getOrElse("").replaceAll("/\\d{2,}", "/{id}")}"
    case _ => query.key
  }
}

/** Thrown for a gap: the caller sees a failed call, and the gap count says it was not a failure. */
final class LookupGap(query: String) extends RuntimeException(s"not answered: $query") with NoStackTrace
