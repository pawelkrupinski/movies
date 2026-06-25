# White-cinema investigations

Durable cross-run log for the recurring (~every 3 days) "white cinema"
investigation. A **white** uptime bar = the scrape *fetch succeeded* but the
*parser returned zero showtimes* (parsed-but-empty), as opposed to red/yellow
(the fetch itself threw — 5xx/timeout/TLS/403). Each run targets cinemas whose
**last 3 consecutive active scrape buckets are all white** and records, per
venue, the root cause + action: `fixed` (SHA) / `unfixable: <reason>` /
`intentionally-dormant` / `needs-human: <reason>` / `recovered`.

Read this before investigating so you don't re-diagnose a venue already settled,
and so you can re-check whether a previously-broken venue has recovered.

## How to find the white list (methodology)

`/uptime` (kinowo.fly.dev) is now **auth-gated** — OAuth login + `ADMIN_ALLOWLIST`
(checked in `web/.../controllers/AdminAction.scala`); an anonymous `curl` returns
`401 "Not logged in."`. So query prod Mongo directly instead:

1. A `flyctl proxy 27017:27017 --app kinowo-mongo` is usually already running;
   start one if not (see the `prod-mongo-access` memory).
2. Connect with `MONGODB_URI` from the root checkout `.env.local`, host swapped to
   `127.0.0.1:27017`.
3. Collection `uptimeBuckets`, docs `{service, bucket, successes, failures,
   zeroes, errors, ...}`. Replicate `UptimeController`'s predicate
   (`web/.../controllers/UptimeController.scala`): per service take the last
   `RecentScrapes = 3` non-empty buckets; status `zero` (white) =
   `zeroes>0 && failures==0 && successes==0`. A venue is 3-scrape-white when those
   3 are all `zero`. Skip `*|enrichment` services. Service name = the cinema's
   `displayName` (`common/.../models/Cinema.scala`); map it to its client in
   `worker/.../services/cinemas/CinemaScraperCatalog.scala`.

---

## 2026-06-25

3 cinemas were 3-scrape-white. **All three are genuinely film-dormant — parsers
verified working; no fixable scraper bug.** No code change shipped this run.

### Kino PDK (Pyrzyce) — `intentionally-dormant`
- Client: `BiletynaClient` @ `https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury`.
- Never green in the 24h record. Live ld+json page has **0 `ScreeningEvent`**;
  the only upcoming item is a November `ComedyEvent` (kabaret), which the parser
  correctly drops as non-movie. Parser working; venue has no films programmed.
- Action: none. Re-check next run — will go green when it lists films again.

### Kino za Rogiem (Płock) — `intentionally-dormant` (transient between repertoires)
- Client: `MsiClient` @ `https://bilety.pokis.pl` (fetches current + next month
  `MSI/mvc/pl?date=YYYY-MM`).
- Was **green 2026-06-24 13:00Z**, flipped white ~same day. Recorded as white
  (zero), not red — so the worker's own fetch succeeds and parses zero films
  (the site times out from a local/residential IP, but the worker reaches it).
  June Jim-Jarmusch cycle ended; July repertoire not yet loaded (July fixture
  already empty). Parser shape matches fixtures.
- Action: none. **Re-check next run — expect recovery when July films load.** If
  still white after ~2 more runs with films visibly on bilety.pokis.pl, suspect a
  real break and re-probe.

### Kino PCA (Polkowice) — `intentionally-dormant`
- Client: `Bilety24OrganizerClient` @
  `https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689`.
- Never green in the 24h record. Organizer page is live and functional but lists
  only theatre + stand-up (`Spektakl:` / `Wydarzenie:` anchors) and **zero
  `Film:` anchors**; the parser keys on `Film:` so it correctly returns empty.
  Had films on 2026-06-08; venue has since stopped programming films.
- Action: none. Re-check next run.
