package tools

import java.util.concurrent.{CompletableFuture, Executors, Semaphore}

/**
 * A capture's live asks run side by side: each lane (a family's questions, a host's pages) at most `atOnce` at a time,
 * every lane at once, each ask on a virtual thread of its own — and a lane's next ask started the moment one of its
 * window finishes, never when the slowest of a batch does.
 *
 * Not `CompletableFuture.runAsync`'s default `ForkJoinPool.commonPool`: its parallelism is the machine's cores less one,
 * three on a 4-vCPU GitHub runner, so the PL identity capture of 2026-10-12 (run 38281127226) asked three families at
 * once and one question of each, waited every host's 30s timeout out alone, and spent 7.5 minutes a round — and a batch
 * (`grouped(n)` joined batch by batch) waited for its slowest ask before the next began.
 */
object ConcurrentAsks {

  /** `ask` of every item of every lane, each lane at most `atOnce` at a time, all lanes at once; returns when all are
   *  done. An ask's failure is rethrown once every ask is done (as `join` throws it) — catch inside `ask` to go on. */
  def inLanes[A](lanes: Iterable[Seq[A]], atOnce: Int)(ask: A => Unit): Unit = {
    val pool = Executors.newVirtualThreadPerTaskExecutor()
    try CompletableFuture.allOf(lanes.toSeq.flatMap { items =>
      val window = new Semaphore(math.max(1, atOnce))
      items.map(item => CompletableFuture.runAsync({ () =>
        window.acquire()
        try ask(item) finally window.release()
      }: Runnable, pool))
    }*).join()
    finally pool.shutdown()
  }

  /** [[inLanes]] of one lane. */
  def each[A](items: Seq[A], atOnce: Int)(ask: A => Unit): Unit = inLanes(Seq(items), atOnce)(ask)
}
