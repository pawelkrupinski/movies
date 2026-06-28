# Kinowo — promotion & growth playbook

A prioritized plan for getting Kinowo (`kinowo.fly.dev` + the iOS / Android apps)
in front of people. Ordered roughly by leverage-per-effort. The technical SEO
backbone (sitemap, structured data, canonicals, query-shaped titles, Core Web
Vitals) shipped alongside this doc — several items below depend on it.

The market is Polish-language, city-by-city. Almost every win is local:
"repertuar kin <miasto>", "co grają w kinie dziś <miasto>", "<tytuł filmu>
seanse <miasto>". The product already covers 40 cities, so the strategy is to
rank for the long tail of (city × film) queries and convert the people who run
them into repeat app users.

---

## 0. Do this first (an afternoon, compounding payoff)

1. **Google Search Console** — verify `kinowo.fly.dev` (the verification token is
   already in the landing `<head>`), then submit `https://kinowo.fly.dev/sitemap.xml`.
   Watch the Coverage / Pages report; fix anything stuck in "Discovered – not
   indexed". This is the single highest-leverage action — it's how the ~1,200
   city+film URLs the sitemap now exposes actually get crawled.
2. **Bing Webmaster Tools** — same drill (Bing also feeds DuckDuckGo + ChatGPT
   search). One-time sitemap submit.
3. **Rich Results Test** — paste a live `/{city}/film?title=…` URL into
   <https://search.google.com/test/rich-results> and confirm the Movie,
   ScreeningEvent and Breadcrumb JSON-LD parse with no errors. Fix any warnings.
4. **Validate the sitemap + robots** — `curl https://kinowo.fly.dev/robots.txt`
   (should list the `Sitemap:` line) and open the sitemap in a browser.

---

## 1. SEO content — the long-tail engine (highest leverage)

The site already targets "repertuar kin <miasto>" via the per-city titles +
descriptions shipped in this change. Next levers, in order:

- **Per-cinema pages** (`/{city}/kino/{cinema-slug}` or similar) targeting
  "kino <nazwa> repertuar" / "<nazwa kina> godziny seansów". Each cinema is its
  own steady search query and currently has no landing page. Highest-value SEO
  build remaining; add the pages to the sitemap when they ship.
- **Internal linking** — make sure every film card links to its detail page
  (it does) and every detail page links back to the city listing + to
  director/cast/genre facet pages (it does, via the meta-link rows). This spreads
  crawl equity to the long tail. Consider linking a film to the *same film in
  other cities* it plays.
- **Fresh-content signal** — listings change daily; the sitemap `lastmod` now
  reflects the read-model mtime, which tells crawlers to re-index often. Keep it
  honest (it's wired to the actual data freshness).
- **Avoid thin/duplicate pages** — the canonical tags already fold the
  `/filmy` alias and `?filter` variations into one URL; don't add indexable
  filtered permutations.

## 2. App Store Optimization (ASO)

The iOS + Android apps are live and the website already declares the
AASA / assetlinks association, so web → app deep links work.

- **Keywords / title / subtitle** — target "repertuar kin", "seanse", "kino",
  "bilety do kina", city names. Polish-localize the store listing fully.
- **Screenshots** — lead with the two-per-row showtime card + ratings (the
  product's distinctive view), one screenshot per key flow (city pick, listing,
  film detail, plan).
- **Smart App Banner / install prompt** on the mobile website (`<meta name=
  "apple-itunes-app">` for iOS; a dismissible install nudge for Android) to
  convert mobile-web visitors — who arrive via the SEO long tail — into app
  installs.
- **Ratings prompt** in-app after a few sessions; store rating volume is the
  biggest ASO ranking factor.

## 3. Polish social & community channels

- **Wykop.pl** — the launch-friendly PL community. A "zrobiłem agregator
  repertuarów wszystkich kin w 40 miastach" post with a share card converts well
  if framed as a useful free tool, not an ad.
- **Reddit** — `r/Polska`, plus city subs (`r/Poznan`, `r/Wroclaw`,
  `r/warszawa`, `r/krakow`, …) and `r/postacie`/film fan subs. Post the local
  city URL in the relevant city sub ("repertuar wszystkich kin w Poznaniu w
  jednym miejscu").
- **Facebook groups** — "Co robić w <mieście>", student groups, cinema-fan and
  kino-studyjne groups. The OG share cards (poster + ratings composite) are
  already built and make link posts look native.
- **Telegram / Discord** — film and city community servers.

Tactic: don't spray the same link everywhere. Post each city's URL in that
city's community, with copy in that city's voice.

## 4. Growth loops (turn usage into reach)

- **Share button prominence** — the server-rendered 1200×630 OG card per film is
  a strong, ready-made asset. Make "udostępnij" obvious on the film page and in
  the apps so users spread links that preview beautifully.
- **Weekly digest** — "Co w kinie w <mieście> w ten weekend": a Telegram channel
  / email per city. The Telegram bot infrastructure already exists (used for
  alerts) and could be repurposed — flagged as a future build, not in this pass.
- **Plan sharing** — the `/plan` feature produces a shareable multi-film plan;
  surface a share affordance there too.

## 5. Backlinks & partnerships (authority)

- **Local city portals** — "co robić w <mieście>" / event-listing sites are
  often happy to link a free, comprehensive repertoire tool.
- **Cinema websites** — small independent cinemas (the ones already scraped) may
  link Kinowo as an "also see our listing on…" — a natural, relevant backlink.
- **Film blogs / newsletters** — a tool roundup mention.
- **Event aggregators / Google "Things to do"** — the new ScreeningEvent
  structured data makes screenings eligible for Google's event surfaces; this is
  a passive channel that grows as crawling catches up.

## 6. Paid / launch spikes (optional, later)

- **Product Hunt** — limited PL reach but good for the tech crowd + a backlink.
- **Google Ads on city keywords** — only if there's budget; organic should carry
  most of the long tail once indexed. If tried, bid on "repertuar kin <miasto>"
  and send to the matching city page.

---

## Measurement

- **Search Console**: impressions / clicks per query and per page; which city &
  film URLs rank. The north-star SEO metric.
- **Plausible/GA or server logs**: organic landing pages, city distribution,
  web→app conversion.
- **App stores**: installs, retention, rating volume.

Re-check Search Console Coverage ~2 weeks after the sitemap submit to confirm the
film URLs are getting indexed; that's the signal the whole SEO backbone is
working.
