# 4. `LiveCorpus` memos (`heads`, `records`, candidates)

- **Where:** `LiveCorpus`
- **Verdict:** (a) cache, given the event feed

`LiveCorpusSpec` checks equality with `CorpusContext.of` after every event. Gap: a detail page or record filed WITHOUT its `AnswersChanged` event stays stale until a restart. That is a feed-delivery property, not stored state.
