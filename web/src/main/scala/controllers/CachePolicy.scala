package controllers

/**
 * What a cache — the visitor's browser, and Cloudflare in front of us — may do
 * with one of the conditional responses `MovieController.conditionalGzipped`
 * builds.
 *
 * Every one of them carries a STRONG per-city ETag derived from
 * `WebReadModel.lastModifiedFor`, and that is what makes the revalidating
 * policies exact rather than approximate: the validator moves when, and only
 * when, the bytes that city renders can have changed.
 */
enum CachePolicy {

  /** `private, no-cache` — the browser keeps a copy and revalidates before
   *  every reuse; no shared cache may hold it at all. For pages that are
   *  client-independent but too numerous to be worth an edge entry each (a
   *  `?filter=` variant is one URL per filter per city). */
  case BrowserOnly

  /** `public, max-age=0, must-revalidate` — ANY cache, the edge included, may
   *  store the body, and NONE may reuse it without revalidating first.
   *
   *  NO TTL, DELIBERATELY. An `s-maxage` is a guess that the bytes will still
   *  be good in N seconds; the ETag is the actual answer. Against a validator
   *  this precise a TTL buys nothing but staleness — a city's page can change
   *  a second after the edge stored it, and a clock cannot know — while
   *  revalidating costs the origin only a 304, which short-circuits before the
   *  page is rendered or gzipped at all. The edge still saves the whole body
   *  on the wire; it just never guesses about freshness.
   *
   *  Verified against the live edge: a response carrying this answers
   *  `cf-cache-status: REVALIDATED` — Cloudflare kept the bytes, asked us, took
   *  the 304, and served its own copy.
   *
   *  ⚠️ ONLY FOR RESPONSES THAT ARE BYTE-IDENTICAL FOR EVERY CLIENT, because a
   *  shared cache hands one visitor's copy to the next. The bare city listing
   *  and the public JSON payloads qualify: no template they reach accepts a
   *  `models.User`, and `MovieController` holds no `UserRepository`, so no
   *  session cookie can move a byte of them. What is per-user went the other way
   *  instead — `/api/me` and `/api/me/state` answer about one person and say
   *  `private, no-store` (`PerUserResponse`), and `shared.js` layers their
   *  answer onto the cached page after first paint. Filtered listings, facet
   *  pages and film pages stay `BrowserOnly`: client-independent too, but one
   *  URL per filter per city is not worth an edge entry each. A shared cache in
   *  front of anything per-user would serve one visitor's state to another.
   *
   *  ⚠️ AND NO `Set-Cookie` ON SUCH A RESPONSE. Cloudflare bypasses the cache
   *  for anything carrying one — measured, the listing went `DYNAMIC` ->
   *  `BYPASS` with the `city=` cookie the only thing left on it. `shared.js`
   *  writes that cookie now. */
  case RevalidatedAnywhere

  /** No `Cache-Control` at all; the client manages its own revalidation. */
  case Unset
}
