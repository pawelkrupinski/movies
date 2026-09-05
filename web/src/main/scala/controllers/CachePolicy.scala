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
   *  on the wire; it just never guesses about freshness. */
  case RevalidatedAnywhere

  /** `public, max-age=0, s-maxage=N` — a shared cache may answer for N seconds
   *  without asking us. Carries real staleness, so it is only for payloads
   *  where absorbing origin load has been measured to be worth that. */
  case EdgeTtl

  /** No `Cache-Control` at all; the client manages its own revalidation. */
  case Unset
}
