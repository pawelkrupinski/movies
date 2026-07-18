package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.{FalenicaClient, SdkClient}

object WriteFalenicaSdk {
  def main(args: Array[String]): Unit = {
    println("=== Falenica ===")
    new FalenicaClient(new RecordingHttpFetch("kino-falenica", new RealHttpFetch())).fetch().foreach(println)
    println("=== SDK ===")
    new SdkClient(new RecordingHttpFetch("kino-sdk", new RealHttpFetch())).fetch().foreach(println)
  }
}
