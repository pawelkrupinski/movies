package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Try

/**
 * A KEPT JVM's request loop (`scripts/identity-slice --serve`, `scripts/identity-what-if`): a tool loads what is dear to
 * load once -- a recording opened, a corpus and its context -- and serves request after request over a Unix socket at
 * `socket`, until `idleMinutes` pass with none. A request is its arguments, NUL-separated, behind their length (a 4-byte
 * big-endian int); its answer is everything the handler prints to `Console.out` (and to `System.err` too with
 * `errToClient`), then `\u0000EXIT <code>\n`. One request at a time: the handler holds the JVM's whole state.
 */
object KeptJvm {

  def serve(socket: Path, idleMinutes: Int, label: String, errToClient: Boolean = false)(request: List[String] => Int): Unit = {
    import java.nio.channels.{Channels, ServerSocketChannel}
    Files.createDirectories(socket.toAbsolutePath.getParent)
    Files.deleteIfExists(socket)
    val server = ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX)
    server.bind(java.net.UnixDomainSocketAddress.of(socket))
    // Since the last request ended; None while one is being served.
    val idleSince = new java.util.concurrent.atomic.AtomicReference(Option(Stopwatch.start()))
    val idle = idleMinutes * 60000L
    val watchdog = new Thread(() => while (server.isOpen) {
      Thread.sleep(5000)
      if (idleSince.get.exists(_.millis > idle)) { Files.deleteIfExists(socket); server.close() }
    })
    watchdog.setDaemon(true)
    watchdog.start()
    System.err.println(s"[$label] serving on $socket, stopping after $idleMinutes idle minute(s)")
    val ownErr = System.err
    try while (true) {
      val client = server.accept()
      idleSince.set(None)
      try {
        val in     = new java.io.DataInputStream(Channels.newInputStream(client))
        val args   = new String(in.readNBytes(in.readInt()), StandardCharsets.UTF_8).split('\u0000').toList.filter(_.nonEmpty)
        val answer = new java.io.PrintStream(Channels.newOutputStream(client), true, StandardCharsets.UTF_8)
        if (errToClient) System.setErr(answer)
        val code = try Try(Console.withOut(answer)(request(args))).recover { case e =>
          e.printStackTrace(answer); answer.println(s"[$label] the request failed: $e"); 1 }.get
        finally System.setErr(ownErr)
        answer.print(s"\u0000EXIT $code\n")
        answer.flush()
      } catch { case e: java.io.IOException => ownErr.println(s"[$label] a client went away: $e") }
      finally { client.close(); idleSince.set(Some(Stopwatch.start())) }
    } catch { case _: java.nio.channels.ClosedChannelException => ownErr.println(s"[$label] idle: stopped") }
    finally Files.deleteIfExists(socket)
  }
}
