package tools

import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.util.concurrent.ConcurrentHashMap

/**
 * A byte pipe from a local port to `host:targetPort` that a spec can cut and restore — Mongo going
 * away and coming back, without touching the real server (the shared local `:28017` must never be
 * restarted by a spec).
 *
 * [[sever]] closes every open connection and refuses new ones (accepted, then closed at once), so
 * the driver sees exactly what a dead primary looks like from the client: reset sockets and failed
 * server selection. [[restore]] answers again. One thread pair per connection, daemon threads.
 */
final class TcpForwarder private (server: ServerSocket, host: String, targetPort: Int) extends AutoCloseable {
  @volatile private var severed = false
  private val open = ConcurrentHashMap.newKeySet[Socket]()

  /** The local port to dial. */
  def port: Int = server.getLocalPort

  /** Drop every connection and refuse new ones until [[restore]]. */
  def sever(): Unit = { severed = true; open.forEach(socket => closeQuietly(socket)); open.clear() }

  /** Answer again. */
  def restore(): Unit = severed = false

  override def close(): Unit = { closeQuietly(server); sever() }

  private def start(): TcpForwarder = {
    TcpForwarder.daemon("tcp-forwarder-accept") {
      try while (true) {
        val client = server.accept()
        if (severed) closeQuietly(client)
        else {
          val upstream = new Socket(host, targetPort)
          open.add(client); open.add(upstream)
          pipe(client, upstream); pipe(upstream, client)
        }
      } catch { case _: java.io.IOException => () }
    }
    this
  }

  private def pipe(from: Socket, to: Socket): Unit = TcpForwarder.daemon("tcp-forwarder-pipe") {
    try from.getInputStream.transferTo(to.getOutputStream) catch { case _: java.io.IOException => () }
    finally { closeQuietly(from); closeQuietly(to); open.remove(from); open.remove(to) }
  }

  private def closeQuietly(socket: java.io.Closeable): Unit = try socket.close() catch { case _: java.io.IOException => () }
}

object TcpForwarder {
  /** Forward a port of its own — bound by the system and HELD for the forwarder's whole life — to
   *  `host:targetPort`. There is no "find a free port, release it, bind it later": between the
   *  release and the bind any other process on this machine (another agent's test, a dev server)
   *  could take it. A spec that needs the port dead first starts the forwarder and [[sever]]s it. */
  def start(host: String, targetPort: Int): TcpForwarder = {
    val server = new ServerSocket()
    server.bind(new InetSocketAddress("127.0.0.1", 0))
    new TcpForwarder(server, host, targetPort).start()
  }

  private def daemon(name: String)(body: => Unit): Unit = {
    val thread = new Thread(() => body, name); thread.setDaemon(true); thread.start()
  }
}
