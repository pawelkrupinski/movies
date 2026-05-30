import play.sbt.PlayRunHook
import sbt.io.syntax._
import sbt.File

import java.net.Socket
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

/**
 * Before `sbt run` starts the Play app, make sure `127.0.0.1:27017`
 * resolves to a real Mongo. The self-hosted Mongo lives on Fly's private
 * `.internal` network, only reachable from a laptop via
 * `flyctl proxy 27017:27017 --app kinowo-mongo`. Forgetting to start that
 * proxy looks like a stranded-empty-cache bug from the browser side
 * (`Brak repertuaru`) — see `MongoConnection`'s init-failed log for the
 * shape.
 *
 * Behaviour:
 *
 *  - **`.env.local` not pointing at a local host** → no-op. Atlas-style
 *    URIs (anything not containing 127.0.0.1 / localhost) handle their own
 *    connectivity; nothing to start.
 *  - **Port 27017 already listening** → no-op. The developer has their
 *    own proxy / mongod running; we leave it alone.
 *  - **Otherwise** → fork `flyctl proxy 27017:27017 --app kinowo-mongo`
 *    in the background, poll the port until it's reachable (or 30 s),
 *    then proceed. `afterStopped` kills the process so a Ctrl-C on sbt
 *    cleans up without leaving an orphan tunnel.
 *
 * No effect in prod — `playRunHooks` only fire under `sbt run`, which the
 * Docker image never invokes (it starts the packaged Play app directly).
 */
object MongoProxy {

  private val Port      = 27017
  private val WaitMs    = 30000L
  private val PollMs    = 200L
  private val FlyApp    = "kinowo-mongo"

  def apply(baseDir: File): PlayRunHook = new PlayRunHook {

    @volatile private var proxyProcess: Option[Process] = None

    override def beforeStarted(): Unit = {
      if (!localUriConfigured(baseDir)) {
        // Atlas / remote URI: nothing to forward.
        return
      }
      if (isPortListening(Port)) {
        println(s"[MongoProxy] :$Port already listening — assuming a tunnel or local mongod is up; not starting flyctl.")
        return
      }
      println(s"[MongoProxy] starting `flyctl proxy $Port:$Port --app $FlyApp` in the background…")
      val sink = ProcessLogger(_ => (), _ => ())
      val started = Try {
        Process(Seq("flyctl", "proxy", s"$Port:$Port", "--app", FlyApp)).run(sink)
      }
      started.toOption match {
        case Some(p) => proxyProcess = Some(p)
        case None    =>
          println("[MongoProxy] failed to launch flyctl (not installed / not authenticated?). Mongo will be unreachable; cache will hydrate via scrapes only.")
          return
      }
      val deadline = System.currentTimeMillis() + WaitMs
      while (System.currentTimeMillis() < deadline && !isPortListening(Port)) {
        Thread.sleep(PollMs)
      }
      if (isPortListening(Port)) println(s"[MongoProxy] tunnel ready on 127.0.0.1:$Port.")
      else                       println(s"[MongoProxy] tunnel didn't open within ${WaitMs / 1000}s — boot will likely render an empty repertoire until the next scrape tick.")
    }

    override def afterStopped(): Unit = {
      proxyProcess.foreach { p =>
        println("[MongoProxy] stopping the flyctl proxy.")
        p.destroy()
      }
      proxyProcess = None
    }
  }

  /** True when `.env.local`'s active `MONGODB_URI` points at this machine
   *  — that's the signal the developer is relying on a tunnel. The
   *  commented-out Atlas fallback line is skipped (`#` prefix). */
  private def localUriConfigured(baseDir: File): Boolean = {
    val envFile = baseDir / ".env.local"
    if (!envFile.exists()) return false
    Try {
      val src   = scala.io.Source.fromFile(envFile, "UTF-8")
      val lines = try src.getLines().toList finally src.close()
      lines.exists { l =>
        val trimmed = l.trim
        trimmed.startsWith("MONGODB_URI=") && (trimmed.contains("127.0.0.1") || trimmed.contains("localhost"))
      }
    }.getOrElse(false)
  }

  private def isPortListening(port: Int): Boolean = {
    val sock = new Socket()
    try {
      sock.connect(new java.net.InetSocketAddress("127.0.0.1", port), 200)
      true
    } catch {
      case _: Throwable => false
    } finally Try(sock.close())
  }
}
