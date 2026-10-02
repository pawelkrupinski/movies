package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.MultikinoClient
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Refresh the Multikino fixture. `RecordingHttpFetch` writes every
 *  response body the client touches under `test/resources/fixtures/multikino/`,
 *  so simply running `MultikinoClient.fetch()` through it captures the API
 *  response (and the homepage, if the session warm-up fires) without any
 *  bespoke recording code here. */
object WriteMultikino {
  def main(args: Array[String]): Unit = {
    tools.ProxyTunnelAuthentication.BasicAllowed.applyToJvm()
    val process = _root_.settings.ProcessConfiguration.resolve()
    val shards  = modules.wiring.EgressWiring.residentialShards(tools.ResidentialProxy.fromConfiguration(process), tools.TlsTrust.newContext())
    // Recording OUTSIDE the chain, so a proxy- or Zyte-served body is captured too; Zyte only
    // behind the proxy, as in production (modules.wiring.EgressWiring.paidEgressChain).
    val fetch  = new RecordingHttpFetch("multikino", modules.wiring.EgressWiring.multikinoChain(process, shards, new RealHttpFetch()))
    val client = new MultikinoClient(fetch, titles = titleNormalizer)
    client.fetch().foreach(m => println(s"${m.movie.title} (${m.showtimes.size} showtimes)"))
  }
}
