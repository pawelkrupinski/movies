package services.identity
package adapters

import services.identity.agreement.AnswerFeed

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{ConcurrentSkipListMap, CopyOnWriteArrayList}
import scala.jdk.CollectionConverters._

/**
 * The [[AnswerFeed]] of one [[IdentityTmdbLayer]]: its TMDB store, its family-answer store and the worker's venue page
 * index publish here, the model subscribes, and the agreement stage counts its own keys through a view. Nothing is kept
 * but each view's log of the keys it accepted.
 */
final class FiledAnswerFeed(kept: Int = FiledAnswerFeed.Kept) extends AnswerFeed {
  private val views     = new CopyOnWriteArrayList[FiledAnswerFeed.Log]()
  private val listeners = new CopyOnWriteArrayList[String => Unit]()

  def publish(key: String): Unit = {
    views.forEach(_.offer(key))
    listeners.forEach(_(key))
  }

  def subscribe(listener: String => Unit): Unit = { listeners.add(listener); () }

  def view(read: String => Option[String]): AnswerFeed.View = {
    val log = new FiledAnswerFeed.Log(read, kept)
    views.add(log)
    log
  }
}

object FiledAnswerFeed {
  /** How many filings a view's [[Log.changedSince]] can name, unless the feed says otherwise: a burst past it re-reads
   *  the verdicts' answers. */
  val Kept = 100000

  /** A view's own numbering of the filings it accepts, each under the key `read` names it by, by the version it made;
   *  the oldest dropped past `kept`, a version before them then unknown. */
  private[adapters] final class Log(read: String => Option[String], kept: Int) extends AnswerFeed.View {
    private val filed   = new AtomicLong()
    private val changes = new ConcurrentSkipListMap[Long, String]()

    def version: Long = filed.get

    private[FiledAnswerFeed] def offer(key: String): Unit = read(key).foreach { named =>
      changes.put(filed.incrementAndGet(), named)
      while (changes.size > kept) changes.pollFirstEntry()
      ()
    }

    /** The keys filed after `version`, or `None` once filings that old are no longer kept. */
    def changedSince(version: Long): Option[Set[String]] =
      Option.when(changes.isEmpty || version >= changes.firstKey - 1 || version >= filed.get)(
        changes.tailMap(version, false).values.asScala.toSet)
  }
}
