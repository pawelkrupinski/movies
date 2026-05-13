package clients.tools

import clients.KinoBulgarskaClient
import tools.RealHttpFetch

object WriteKinoBulgarska extends App {
  private val client = new KinoBulgarskaClient(new RecordingHttpFetch("kino-bulgarska", new RealHttpFetch()))
  client.fetch().foreach(println)
}
