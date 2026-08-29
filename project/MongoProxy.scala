import play.sbt.PlayRunHook
import sbt.io.syntax._
import sbt.File

import java.net.Socket
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

/**
 * Before `sbt run` starts the Play app, make sure `127.0.0.1:27017`
 * resolves to a real Mongo. The self-hosted Mongo binds loopback and a
 * private subnet on its own host, so a laptop reaches it only through a
 * tunnel. Forgetting to start one looks like a stranded-empty-cache bug
 * from the browser side (`Brak repertuaru`) — see `MongoConnection`'s
 * init-failed log for the shape.
 *
 * This used to fork `flyctl proxy --app kinowo-mongo`. Prod moved off that
 * Fly app onto the Hetzner host mongo-1 on 2026-08-29 and the app is now
 * STOPPED, so the tunnel is an ssh local forward instead. The shell scripts
 * that also need prod share one definition of this in
 * `scripts/local-mirror/prod-tunnel.sh`; sbt build code cannot source a bash
 * file, so the target and the flags are restated here — keep the two in step.
 *
 * Behaviour:
 *
 *  - **`.env.local` not pointing at a local host** → no-op. Atlas-style
 *    URIs (anything not containing 127.0.0.1 / localhost) handle their own
 *    connectivity; nothing to start.
 *  - **Port 27017 already listening** → no-op. The developer has their
 *    own tunnel / mongod running; we leave it alone.
 *  - **Otherwise** → fork `ssh -N -L 27017:127.0.0.1:27017 <target>`
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

  /** mongo-1's public address, as pinned in `infra/nix/hosts/mongo-1/default.nix`
   *  and mirrored by `scripts/local-mirror/prod-tunnel.sh`. Overridable with
   *  `KINOWO_MONGO_SSH` in `.env.local` so a rescue host needs no code change. */
  private val DefaultSshTarget = "root@2.28.56.140"

  def apply(baseDir: File): PlayRunHook = new PlayRunHook {

    @volatile private var proxyProcess: Option[Process] = None

    override def beforeStarted(): Unit = {
      if (!localUriConfigured(baseDir)) {
        // Atlas / remote URI: nothing to forward.
        return
      }
      if (isPortListening(Port)) {
        println(s"[MongoProxy] :$Port already listening — assuming a tunnel or local mongod is up; not starting one.")
        return
      }
      val target = sshTarget(baseDir)
      println(s"[MongoProxy] starting `ssh -N -L $Port:127.0.0.1:$Port $target` in the background…")
      val sink = ProcessLogger(_ => (), _ => ())
      val started = Try {
        // ExitOnForwardFailure: ssh will otherwise hold a session open with NO
        // forward when the bind fails, which polls as "not listening" for the
        // full 30s and then leaks. BatchMode: fail now rather than block sbt on
        // a passphrase prompt nobody is watching for.
        Process(Seq(
          "ssh", "-N", "-L", s"$Port:127.0.0.1:$Port",
          "-o", "ExitOnForwardFailure=yes", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
          "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=3",
          target
        )).run(sink)
      }
      started.toOption match {
        case Some(p) => proxyProcess = Some(p)
        case None    =>
          println(s"[MongoProxy] failed to launch ssh to $target (no key / host unreachable?). Mongo will be unreachable; cache will hydrate via scrapes only.")
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
        println("[MongoProxy] stopping the ssh tunnel.")
        p.destroy()
      }
      proxyProcess = None
    }
  }

  /** Where to tunnel to. `.env.local`'s `KINOWO_MONGO_SSH` wins so the host can
   *  move (or a rescue box stand in) without touching build code; otherwise the
   *  pinned default. */
  private def sshTarget(baseDir: File): String =
    envValue(baseDir, "KINOWO_MONGO_SSH").filter(_.nonEmpty).getOrElse(DefaultSshTarget)

  /** `.env.local`, trimmed line by line — deliberately NOT a `source`, since the
   *  Mongo URIs there carry `&`/`?`. Empty when the file is absent, so both
   *  readers below degrade to "not configured" rather than throwing. */
  private def envLines(baseDir: File): List[String] = {
    val envFile = baseDir / ".env.local"
    if (!envFile.exists()) return Nil
    Try {
      val src = scala.io.Source.fromFile(envFile, "UTF-8")
      try src.getLines().map(_.trim).toList finally src.close()
    }.getOrElse(Nil)
  }

  /** Read one `KEY=VALUE`. Commented-out lines never match — the `#` is part of
   *  the prefix test. */
  private def envValue(baseDir: File, key: String): Option[String] =
    envLines(baseDir).find(_.startsWith(s"$key=")).map(_.drop(key.length + 1).trim)
      .map(v => v.stripPrefix("\"").stripSuffix("\"").stripPrefix("'").stripSuffix("'"))

  /** True when `.env.local`'s active `MONGODB_URI` points at this machine
   *  — that's the signal the developer is relying on a tunnel. The
   *  commented-out Atlas fallback line is skipped (`#` prefix). */
  private def localUriConfigured(baseDir: File): Boolean =
    envValue(baseDir, "MONGODB_URI").exists(u => u.contains("127.0.0.1") || u.contains("localhost"))

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
