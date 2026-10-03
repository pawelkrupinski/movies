/**
 * THE DASHBOARD ANSWERS ONLY ITS OWN NAME. It binds 127.0.0.1, but a bind stops nobody's browser: a
 * page on any domain can re-point that domain's DNS at 127.0.0.1 (DNS rebinding) and then talk to
 * this server same-origin -- run a check, then a switch, with this machine's root ssh to the fleet.
 * Such a request still carries the attacker's domain in its Host header, so anything that is not
 * this loopback address and port is refused before a route sees it.
 */
export function isLocalHost(host: string | undefined, port: number): boolean {
  if (!host) return false;
  const name = host.toLowerCase();
  return name === `127.0.0.1:${port}` || name === `localhost:${port}`;
}

/**
 * A WRITE COMES ONLY FROM THE DASHBOARD'S OWN PAGES. The Host check above does not stop a page on
 * another site posting to http://127.0.0.1:PORT directly: that request names this address in Host,
 * and a `no-cors` POST needs no preflight. The browser does name where it came from -- `Origin` on
 * every cross-origin POST, `Sec-Fetch-Site` on everything modern -- so a non-GET request that says
 * it came from anywhere but this origin is refused. A request carrying neither header (curl, the
 * dashboard's own scripts) is not a browser being driven by another site, and is let through.
 */
export function isOwnOriginWrite(
  method: string,
  origin: string | undefined,
  fetchSite: string | undefined,
  port: number,
): boolean {
  if (method === "GET" || method === "HEAD" || method === "OPTIONS") return true;
  if (fetchSite !== undefined && fetchSite !== "same-origin" && fetchSite !== "none") return false;
  if (origin === undefined) return true;
  const name = origin.toLowerCase();
  return name === `http://127.0.0.1:${port}` || name === `http://localhost:${port}`;
}
