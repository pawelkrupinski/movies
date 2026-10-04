package tools

/**
 * The repository reads [[NoSwallowedFailureSpec]]'s shape 4 would flag — a `Try` around an awaited
 * Mongo read answered with something other than its failure — whose answer is right on its own
 * terms. Each says WHY. Keyed like the spec's own allowlist: (file, enclosing method, the `Try`
 * line trimmed). An entry whose site moved or went fails the spec's stale-entry check.
 */
object RepositoryReadSwallows {

  val Swallows: Map[(String, String, String), String] = Map(
    ("common/src/main/scala/services/MirrorFreshness.scala", "newestIn",
      "Try(Await.result(") ->
      "a dev /debug label's freshness stamp: None reads as 'freshness unknown', which is what a failed read is; logged",
    ("common/src/main/scala/services/MongoCachingDetailFetch.scala", "cached",
      "Try(Await.result(c.find(Filters.and(Filters.eq(\"_id\", idOf(url)), Filters.gt(\"expireAt\", new java.util.Date()))).headOption(), SpecTimeouts.Io))") ->
      "a CACHE read: a miss refetches from the source, the safe direction, and costs one fetch",
    ("common/src/main/scala/services/MongoIndex.scala", "uniqueNow",
      "Try(Await.result(database.getCollection[Document](collection).listIndexes().toFuture(), Timeout)).toOption.exists(_.exists { spec =>") ->
      "asked only after a conversion attempt failed, as 'did a sibling pod convert it meanwhile?': false retries the conversion, which is idempotent",
    ("common/src/main/scala/services/ServiceTags.scala", "load",
      "def load(c: MongoCollection[Document]): Unit = Try {") ->
      "a periodic refresh of an in-memory map: a failed read leaves every entry as it was and the next cycle retries; logged",
    ("common/src/main/scala/services/UptimeSync.scala", "poll",
      "def poll(c: MongoCollection[Document]): Unit = Try {") ->
      "a periodic refresh of an in-memory map: a failed read leaves every entry as it was and the next cycle retries; logged",
    ("common/src/main/scala/services/UptimeSync.scala", "hydrate",
      "def hydrate(c: MongoCollection[Document]): Unit = Try {") ->
      "a boot hydrate of an in-memory map: a failed read leaves it as it was (empty = the documented cold start) and is logged",
    ("common/src/main/scala/services/config/EnvOverrideStore.scala", "refresh",
      "Try(Await.result(c.find().batchSize(tools.MongoReplies.Default).toFuture(), SpecTimeouts.Io)).toOption.foreach { docs =>") ->
      "keeps the overrides it already holds when the read fails — the opposite of reading the failure as 'no overrides'",
    ("common/src/main/scala/services/config/EnvRegistryStore.scala", "publish",
      "Try {") ->
      "the Try is the write's failure log: a failed read throws before anything is reconciled or written",
    ("common/src/main/scala/services/movies/MovieRepository.scala", "upsert",
      "val collidesWithAnother = identityChanging && Try(Await.result(c.find(Filters.and(") ->
      "the collision pre-check only: a sibling holding the key or tmdbId is still refused by the unique index at the write (handled as IdentityHeld below)",
    ("common/src/main/scala/services/movies/ScreeningsRepository.scala", "storedRow",
      "Try(Await.result(c.find(Filters.eq(\"_id\", idOf(filmId, slotKey))).first().toFuture(), SpecTimeouts.Io))") ->
      "absent and unreadable are both 'cannot prove this write redundant': either way the row is written, the safe direction",
    ("common/src/main/scala/services/movies/SlotsRepository.scala", "storedSlot",
      "Try(Await.result(c.find(Filters.eq(\"_id\", idOf(filmId, slotKey))).first().toFuture(), SpecTimeouts.Io))") ->
      "absent and unreadable are both 'cannot prove this write redundant': either way the row is written, the safe direction",
    ("common/src/main/scala/services/resolution/ResolutionStore.scala", "get",
      "Try(Await.result(c.find(Filters.and(Filters.eq(\"_id\", hintKey), Filters.gte(\"at\", cutoff))).headOption(), SpecTimeouts.Io))") ->
      "a CACHE read: a miss re-resolves from the source, the safe direction",
    ("common/src/main/scala/services/resolution/ResolutionStore.scala", "removeForFilm",
      "Try {") ->
      "a forget: 0 entries forgotten is what a failed read-then-delete did; logged, and the next re-resolve asks the source again",
    ("common/src/main/scala/services/tasks/MongoBulkTaskResultStore.scala", "latest",
      "Try(Await.result(c.find().batchSize(tools.MongoReplies.Default).toFuture(), SpecTimeouts.Io).flatMap(toResult).map(r => r.taskType -> r).toMap)") ->
      "each bulk job's last-outcome line beside the queue view: a failed read must not take the queue view down with it; logged at WARN",
    ("common/src/main/scala/services/tasks/MongoTaskQueue.scala", "claim",
      "Try {") ->
      "None is an idle poll: the worker claims again on its next poll, and nothing is decided from it",
    ("web/src/main/scala/services/auth/MongoAuthExchangeCodeStore.scala", "remove",
      "Try(Await.result(c.findOneAndDelete(Filters.eq(\"_id\", code)).headOption(), timeout))") ->
      "a single-use code whose findOneAndDelete outcome is unknown: answered as unredeemable, the visitor signs in again; a retry could not be honoured safely anyway; logged at WARN",
    ("web/src/main/scala/services/users/UserRepository.scala", "upsert",
      "Try {") ->
      "the user as signed in is answered back when the row write fails — a sign-in is not refused over a profile save; logged",
    ("web/src/main/scala/services/users/UserRepository.scala", "revokeSessions",
      "Try(Option(Await.result(c.findOneAndUpdate(Filters.eq(\"id\", id), Updates.inc(\"sessionVersion\", 1),") ->
      "None is answered 503 'retry' by AuthController.revokeSessions — never read as 'no such user'",
    ("worker/src/main/scala/services/enrichment/OmdbAttemptStore.scala", "get",
      "Try(Await.result(c.find(Filters.eq(\"_id\", filmKey)).headOption(), SpecTimeouts.Io)).toOption.flatten.flatMap { d =>") ->
      "the backoff stamp on the single-film path fails OPEN like `all()`: an unread stamp makes the film eligible, costing one OMDb call, never a missed film",
    ("worker/src/main/scala/services/tasks/ChunkPageMemo.scala", "recall",
      "Try(Await.result(c.find(Filters.eq(\"_id\", id(cinema, key))).first().headOption(), SpecTimeouts.Io)).toOption.flatten.flatMap { d =>") ->
      "a page MEMO: a miss re-parses the page, the safe direction",
    ("worker/src/main/scala/services/tasks/MongoChunkScrapeStore.scala", "startRun",
      "Try {") ->
      "None is 'did not claim the run': the scrape is not started twice, and the next tick tries again; logged"
  )
}
