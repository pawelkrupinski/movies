package clients.tools

import services.cinemas.{FalenicaClient, SdkClient}
import tools.RealHttpFetch

object WriteFalenicaSdk {
  def main(args: Array[String]): Unit = {
    println("=== Falenica ===")
    new FalenicaClient(new RecordingHttpFetch("kino-falenica", new RealHttpFetch())).fetch().foreach(println)
    println("=== SDK ===")
    new SdkClient(new RecordingHttpFetch("kino-sdk", new RealHttpFetch())).fetch().foreach(println)
  }
}
