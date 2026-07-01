import play.sbt.{PlayConsoleInteractionMode, PlayInteractionMode}

import java.util.concurrent.CountDownLatch

/**
 * `run` interaction mode that behaves exactly like Play's default when a real
 * terminal is attached, but parks quietly instead of busy-spinning when there
 * is no console.
 *
 * Play's dev server blocks the `run` task waiting for you to press Enter to
 * stop it (`PlayConsoleInteractionMode.waitForCancel` → `waitForKey` →
 * `waitEOF`), which reads a character from stdin. When the sbt process has no
 * controlling terminal — stdin redirected to `/dev/null`, launched detached /
 * `nohup`'d, or the launching terminal was closed and the process reparented to
 * init — that read returns end-of-stream (`-1`) instantly and forever. Play
 * treats EOF as "keep polling" rather than "give up", so the reader loops as
 * fast as the CPU allows. Each iteration allocates (jline reader + queue polls),
 * so the concurrent GC threads spin cleaning up the garbage too: the JVM sits at
 * ~300% CPU for as long as it lives, doing nothing. (Observed in the wild: a
 * detached `sbt web/run` burned ~31h of CPU over ~2 days.)
 *
 * When a console is present we delegate to the stock mode verbatim, preserving
 * the normal "press Enter to stop" dev-loop. When it is absent we block the run
 * task on a latch that is never counted down: the server stays up as a headless
 * process consuming no CPU, and is stopped the only way it can be without a
 * keyboard — by killing the JVM (SIGTERM / SIGINT / Ctrl-C at the parent).
 */
object HeadlessSafeInteractionMode extends PlayInteractionMode {

  private def hasConsole: Boolean = System.console() != null

  override def waitForCancel(): Unit =
    if (hasConsole) PlayConsoleInteractionMode.waitForCancel()
    else new CountDownLatch(1).await() // park forever, zero CPU; JVM kill stops the server

  override def doWithoutEcho(f: => Unit): Unit =
    if (hasConsole) PlayConsoleInteractionMode.doWithoutEcho(f) else f
}
