---
name: record-client-fixtures
description: How and when to capture a real external-API response as an on-disk fixture and replay it in a test — for TMDB, IMDb, Cinemeta, OMDb, Filmweb, Metacritic, RT and scraped cinema-site clients. Use when adding a client or endpoint, changing how a response is parsed, or reproducing a parser bug from a real payload.
---

# Record fixtures for external-service clients

For clients that hit a real external API (TMDB, IMDb, Cinemeta, OMDb,
Filmweb, Metacritic, RT, scraped cinema sites), strongly consider
capturing a real response as a fixture on disk and writing a test that
replays it through the client. Live HTTP in tests is flaky and slow;
hand-written mock JSON drifts from reality and hides parser bugs the
real payload would catch.

When to record a fixture:

- Adding a new client, or a new endpoint on an existing client.
- Changing how a response is parsed (new field, changed shape,
  tightened validation).
- Hitting a real-world payload that exposed a parser bug — capture that
  exact payload so the bug can't regress.

How:

- Save the raw response under
  `test/resources/fixtures/<service>/<case>.<ext>`. Trim noise (huge
  image arrays, tracking ids) only if it doesn't affect parsing.
- Load from disk and feed the parser/decoder directly, OR stub the HTTP
  layer to return the bytes. No network in tests.
- Name after the scenario (`tmdb_movie_with_no_release_date.json`,
  `rt_404_page.html`), not the date or ticket number.
- For large fixtures, leave a one-line comment in the test pointing to
  the URL/query that produced it.

If the response is trivial or the client is a thin pass-through, write
a smaller unit test instead — but don't skip testing the client.
