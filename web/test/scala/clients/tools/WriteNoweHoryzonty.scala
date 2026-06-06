package clients.tools

import services.cinemas.NoweHoryzontyClient
import tools.RealHttpFetch

object WriteNoweHoryzonty {
  def main(args: Array[String]): Unit = {
    val client = new NoweHoryzontyClient(new RecordingHttpFetch("nowe-horyzonty", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
