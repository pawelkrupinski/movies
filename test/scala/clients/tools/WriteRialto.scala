package clients.tools

import clients.RialtoClient
import tools.RealHttpFetch

object WriteRialto extends App {
  private val client = new RialtoClient(new RecordingHttpFetch("rialto", new RealHttpFetch()))
  client.fetch().foreach(println)
}
