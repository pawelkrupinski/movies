package services.movies

import java.util.concurrent.atomic.AtomicInteger

/** A [[TitleNormalizer]] over `rules` that counts its `sanitize` calls — the unit of cost the
 *  scaling specs bound. `counted` narrows it to the titles a spec cares about. */
final class CountingNormalizer(rules: services.titlerules.TitleRuleSet,
                               counted: String => Boolean = _ => true) extends TitleNormalizer(rules) {
  private val n = new AtomicInteger(0)
  def calls: Int = n.get
  def reset(): Unit = n.set(0)
  override def sanitize(title: String): String = {
    if (counted(title)) n.incrementAndGet()
    super.sanitize(title)
  }
}
