package pl.kinowo.net

import kotlinx.serialization.json.Json

/**
 * How this app reads every JSON document — server responses and its own
 * caches alike: fields this build doesn't know are skipped, so a newer server
 * (or a cache a newer build wrote) never breaks an older app.
 */
val LenientJson = Json { ignoreUnknownKeys = true }
