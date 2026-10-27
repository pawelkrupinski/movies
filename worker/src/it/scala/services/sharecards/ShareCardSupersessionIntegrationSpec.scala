package services.sharecards

import tools.SpecTimeouts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*
import java.nio.file.Files
import scala.concurrent.duration.DurationInt

/** The case of [[ShareCardSupersessionSpec]] that holds the film's lock file from a real separate (python3) process. */
class ShareCardSupersessionIntegrationSpec extends AnyFlatSpec with Matchers {

  // The ask-order check and the rename are two steps, and a check run while another replica sits
  // between its own check and rename used to pass both. Each card write now holds the film's lock
  // file (a POSIX record lock, which another PROCESS honours) across check and rename. Held here by
  // a separate process, as a replica's would be, the write must wait for it.
  "A render of inputs a newer request replaced" should "hold a card write until another process's lock on the film is released" in {
    val rig   = new Rig
    val movie = film()
    val lock  = rig.store.lockFor(rig.store.cardPath(movie._id))
    Files.createDirectories(lock.getParent)
    // The holder keeps the lock until its stdin closes — released the moment the test has
    // seen the write wait, rather than after a fixed sleep.
    val holder = new ProcessBuilder("python3", "-c",
      "import fcntl,sys; f=open(sys.argv[1],'a'); fcntl.lockf(f, fcntl.LOCK_EX); print('locked', flush=True); sys.stdin.read()",
      lock.toString).redirectErrorStream(true).start()
    try {
      new java.io.BufferedReader(new java.io.InputStreamReader(holder.getInputStream)).readLine() shouldBe "locked"

      val written = new java.util.concurrent.CountDownLatch(1)
      val writer  = new Thread(() => { rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm), askedAt = Some(T0)); written.countDown() })
      writer.start()
      written.await(SpecTimeouts.quiet(1.second).toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe false     // waiting on the other process
      holder.getOutputStream.close()
      holder.waitFor()
      written.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
      rig.service.existing(rig.service.inputs(movie)) shouldBe defined
    } finally holder.destroy()
  }
}
