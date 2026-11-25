package tools

/** Runs a store's start-up work (an index build, a hydrate) on its own daemon thread, so constructing
 *  the store never blocks boot on Mongo. The body owns its failure handling (each warns in its own words). */
object OffThreadInit {

  /** Starts `body` on a daemon thread called `name`. */
  def start(name: String)(body: => Unit): Unit = {
    val thread = new Thread(() => body, name)
    thread.setDaemon(true)
    thread.start()
  }
}
