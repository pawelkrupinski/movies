/** What Node's fetch throws: a bare "fetch failed" whose cause carries the code. */
export function fetchFailed(code: string, message = `connect ${code}`): TypeError {
  return new TypeError("fetch failed", { cause: Object.assign(new Error(message), { code }) });
}
export const dnsFailure = () => fetchFailed("ENOTFOUND", "getaddrinfo ENOTFOUND api.appstoreconnect.apple.com");
