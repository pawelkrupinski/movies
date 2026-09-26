package tools

import settings.JavaHome

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/** The JDK's own `keytool`, for specs that mint certificates and key stores — the JDK has no
 *  public API for minting a certificate. Fails loudly, with keytool's output, on a non-zero exit. */
final class Keytool(javaHome: JavaHome) {

  def run(args: String*): Unit = {
    val process = new ProcessBuilder((javaHome.binary("keytool").toString +: args)*).redirectErrorStream(true).start()
    val output  = new String(process.getInputStream.readAllBytes(), UTF_8)
    require(process.waitFor(60, TimeUnit.SECONDS) && process.exitValue() == 0, s"keytool ${args.head} failed: $output")
  }
}
