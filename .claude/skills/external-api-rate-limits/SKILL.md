---
name: external-api-rate-limits
description: Concurrency and rate-limit budgets for scripts that hit external services — TMDB, IMDb, Filmweb, Metacritic, Rotten Tomatoes, OMDb, Cinemeta, Mongo, scraped cinema sites. Use when writing or tuning a script that makes many HTTP round-trips, or when a run starts getting 429/503 responses.
---

# Parallelize scripts, but don't get rate-limited

Long-running scripts that hit external services (TMDB, IMDb, Filmweb,
Metacritic, RT, OMDb, Cinemeta, Mongo, scraped cinema sites) should run
per-row work in parallel — serial loops of hundreds of HTTP round-trips
are unacceptably slow when 90% of the time is network wait.

Default to a fixed-thread pool of **5–10 concurrent workers** for scripts
hitting a single API. Stay well under each service's limit:

- TMDB: ~50 req/s — 10 workers is fine.
- IMDb / Cinemeta / RT / Metacritic: undocumented; assume a few hundred
  per minute. 5–10 workers fine; back off on any 429/503.
- Filmweb: undocumented; 5 workers comfortable, more risks soft-blocks.
- OMDb (free tier): 1000 req/day — sequential is fine; the limit is
  daily, not per-second.

On HTTP 429 / 503 / `Request limit reached!`, halve concurrency and add
a small sleep between retries. Don't push harder — the host is telling
you to stop.

For Mongo, parallelism doesn't matter much (the driver pools
connections); use the default unless the script does CPU work between
queries.

Always print throughput at the end (`done in 12.3s, ~8 req/s`) so the
next run can be tuned.
