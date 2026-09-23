package tools

import modules.wiring.EgressWiring
import services.cinemas.roster.ChainDirectory
import services.cinemas.roster.RosterFinding.DirectoryNotRead

/**
 * How [[RosterAudit]] reaches the chains' own venue lists. Multikino's sits
 * behind Cloudflare, which refuses GitHub's runner addresses (403 on both the
 * home page and the list, run 35910520576), so when the Decodo credentials are
 * set (`KINOWO_PROXY_USER`/`PASS`, the worker's and the OG-card workflow's
 * secrets) every list goes through the same proxy-primary chain the worker's
 * Multikino scrapes use, with the direct fetch behind it. Without them the lists
 * are fetched directly, as before.
 *
 * @param proxyShards one fetch per Decodo pool IP, or None when the proxy isn't
 *                    configured
 */
final class ChainListEgress(direct: HttpFetch, proxyShards: Option[IndexedSeq[HttpFetch]]) {

  def fetchFor(directory: ChainDirectory): String => FetchedPage = {
    val http = proxyShards.fold(direct)(EgressWiring.proxyPrimary(_, direct, directory.warmUpUrl))
    url => FetchedPage(url, http.get(url))
  }

  /** An unread list is a note when only the direct address was tried — a
   *  datacenter IP a chain may refuse — but a failure once the residential proxy
   *  was tried too: then nothing left explains it but the list itself. */
  def judged(unread: DirectoryNotRead): DirectoryNotRead =
    if (proxyShards.isEmpty) unread
    else unread.copy(detail = s"${unread.detail}, through the residential proxy too", failing = true)
}

object ChainListEgress {
  def fromEnv(direct: HttpFetch): ChainListEgress =
    new ChainListEgress(direct, EgressWiring.residentialShards(ResidentialProxy.fromEnv()))
}
