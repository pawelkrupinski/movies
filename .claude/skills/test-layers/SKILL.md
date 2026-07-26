---
name: test-layers
description: Exact commands for every local test layer (sbt testUnit, itAll, PageTest, Playwright, iOS LocalServer, swift test), their module-scoping variants, and the testOnly/-z narrowing that cuts a run from minutes to seconds. Use when running tests locally before reporting work done, or when a run is slower than it needs to be.
---

# The test layers, in full

Run **all** the layers that match the change. Run them **in parallel**
when there are no dependencies (separate `Bash` tool calls in the same
message), but do **not** split the runs into new subagents — keep
everything in the main session so the test output and any follow-up
edits stay in one context. The only acceptable reason to skip a layer
is that the local toolchain is missing (e.g. no full Xcode for
`xcodebuild test`); say so explicitly when reporting the work.

- **`sbt testUnit`** — every module's unit specs in one run; or
  `sbt web/Test/test` / `worker/Test/test` / `common/Test/test` /
  `testkit/Test/test` / `e2e/Test/test` to scope to one module.
  Unit specs for controllers, services, enrichment, clients, models.
- **`sbt itAll`** (or `sbt web/IntegrationTest/test` /
  `worker/IntegrationTest/test`) — `*/src/it/scala/` specs that wire
  fakes + the real cache/repository.
- **`sbt web/PageTest/test`** — `web/src/page/scala/views/PageJsBehaviourSpec`
  drives real Chrome over CDP against Twirl-rendered fixtures. Covers
  `web/src/main/assets/js/`, the inline `<script>` blocks in
  `web/src/main/twirl/views/repertoire.scala.html` /
  `_repertoireView.scala.html`, and the rendered HTML shape those JS
  blocks read.
- **`cd page-tests-playwright && npx playwright test [--project …]`**
  — mobile + desktop × Chromium / WebKit / Firefox / Edge. `--project`
  narrows to one engine; the default `--list` shows which exist.
- **iOS LocalServer** — `sbt 'web/PageTest/runMain tools.FixtureServerMain
  <port-file>'` in one shell, `KINOWO_LOCAL_URL=http://127.0.0.1:$(cat
  <port-file>) swift test --package-path ios --filter LocalServer`
  in another. Exercises the real iOS listing parser against the live
  fixture-server render. (The detail screen no longer parses HTML — it
  reads the `/api/details` JSON — so detail changes are covered by the
  unit suites, not here.)
- **`swift test --package-path ios`** — iOS unit / integration suites
  without the live server. Quicker; required for any change to iOS
  model / parser logic regardless of whether you also need LocalServer.

## Run the narrowest scope you can

`sbt web/PageTest/test` runs the page specs in ~30 s. `sbt 'web/PageTest/testOnly
views.PageJsBehaviourSpec -- -z "card poster link"'` runs 4 in ~6 s.
When iterating on one test, use `testOnly` + the `-z` substring
filter. Same for the main `sbt testUnit` — there are hundreds of unit
specs; `testOnly` to the spec under change cuts a full run from
minutes to seconds.

Note the `web/` project prefix is required — a bare
`PageTest/testOnly …` fails to parse.
