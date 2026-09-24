package tools

import java.util.concurrent.ConcurrentLinkedQueue

/** Proves a change-stream watcher SURVIVES a document its codec cannot decode: open the
 *  watcher, make valid writes until one is delivered (the cursor opens asynchronously, so a
 *  write before that proves nothing), write the malformed document, then one more valid write
 *  — which must still be delivered. A watcher whose cursor the malformed document ended never
 *  delivers it.
 *
 *  `watch(seen)` opens the watcher, calling `seen(id)` for every row it delivers, and returns
 *  the handle that closes it. `writeValid(n)` makes a DIFFERENT valid write each call and
 *  returns the id the watcher will report for it. Returns why the watcher failed, or None. */
object MalformedChangeEventProbe {
  def failure(watch: (String => Unit) => AutoCloseable, writeValid: Int => String, writeMalformed: () => Unit,
              budgetMs: Long = 20000): Option[String] = {
    val seen   = new ConcurrentLinkedQueue[String]()
    val handle = watch(id => { seen.add(id); () })
    try {
      var pass = 0
      val live = Eventually.poll(60000, pollMs = 1) {
        pass += 1
        val id = writeValid(pass)
        Eventually.poll(1000, pollMs = 20)(seen.contains(id))
      }
      if (!live) Some("the watcher never delivered a warm-up write, so the probe proves nothing")
      else {
        writeMalformed()
        val after = writeValid(pass + 1000)
        if (Eventually.poll(budgetMs)(seen.contains(after))) None
        else Some(s"the valid write after the malformed document ($after) was never delivered — the stream is dead")
      }
    } finally handle.close()
  }
}
