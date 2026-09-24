package pl.kinowo

import kotlinx.coroutines.NonCancellable
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.withContext
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.HiddenFilmsFetchResult
import pl.kinowo.auth.HiddenFilmsState
import pl.kinowo.auth.LanguageClient
import pl.kinowo.auth.LanguagePushRefused
import pl.kinowo.data.HiddenFilmsOp
import pl.kinowo.data.SyncPrefs
import pl.kinowo.model.Country
import java.io.IOException

// In-memory stand-ins for StateSyncService's collaborators, shared by its
// example-based test and its model-based one.

internal class FakeSyncPrefs : SyncPrefs {
    val hiddenByCountry = mutableMapOf<String, Set<String>>()
    val countryState = MutableStateFlow<String?>(null)
    val languageState = MutableStateFlow<String?>(null)
    private val migrated = mutableSetOf<String>()
    private val etags = mutableMapOf<String, String?>()
    private val lastModifieds = mutableMapOf<String, String?>()

    /** The set of whichever country is selected — what the UI would show. */
    val hiddenState: Set<String> get() = hiddenByCountry[Country.byCode(countryState.value).code] ?: emptySet()

    override val selectedCountryCode = countryState

    override suspend fun hiddenFilmsFor(country: String): Set<String> = hiddenByCountry[country] ?: emptySet()
    override suspend fun setHiddenFilms(country: String, films: Set<String>) { hiddenByCountry[country] = films }

    override suspend fun isHiddenFilmsMigrated(country: String): Boolean = country in migrated
    override suspend fun setHiddenFilmsMigrated(country: String, migrated: Boolean) {
        if (migrated) this.migrated.add(country) else this.migrated.remove(country)
    }

    override suspend fun hiddenFilmsEtag(country: String): String? = etags[country]
    override suspend fun hiddenFilmsLastModified(country: String): String? = lastModifieds[country]
    override suspend fun setHiddenFilmsValidators(country: String, etag: String?, lastModified: String?) {
        etags[country] = etag
        lastModifieds[country] = lastModified
    }

    override suspend fun clearHiddenFilmsSyncState() {
        pendingOps.clear()
        migrated.clear()
        etags.clear()
        lastModifieds.clear()
    }

    override val selectedLanguageTag = languageState
    override suspend fun setLanguageTag(tag: String) { languageState.value = tag }

    private val pendingOps = mutableMapOf<String, List<HiddenFilmsOp>>()
    override suspend fun pendingHiddenFilmsOps(country: String): List<HiddenFilmsOp> = pendingOps[country] ?: emptyList()
    override suspend fun setPendingHiddenFilmsOps(country: String, ops: List<HiddenFilmsOp>) { pendingOps[country] = ops }

    private var pendingLanguage: String? = null
    override suspend fun pendingLanguagePush(): String? = pendingLanguage
    override suspend fun setPendingLanguagePush(tag: String?) { pendingLanguage = tag }
}

/** The per-country hidden-films endpoints as the server behaves: each bucket
 *  is a set, every write applies to it and answers with the resulting set and
 *  a validator derived from its content, a conditional fetch naming the
 *  current validator is a 304, a signed-out session is refused, and an
 *  offline one fails. Tests change the account by editing [remote] — as
 *  another device would — never by scripting responses. Mirrors iOS
 *  `FakeHiddenFilmsClient`. */
internal class FakeHiddenFilmsClient : HiddenFilmsClient {
    val remote = mutableMapOf<String, Set<String>>()
    val hideCalls = mutableListOf<Pair<String, String>>() // (title, country)
    val unhideCalls = mutableListOf<Pair<String, String>>()
    val clearCalls = mutableListOf<String>()
    val fetchCalls = mutableListOf<String>()
    var shouldFailFetch = false
    /** Fail every hide/unhide/clear AFTER recording the call. */
    var shouldFailWrite = false
    /** Whether the session this client sends is signed in. A signed-out one is
     *  answered 401 — which the real client throws on — for every call. */
    var signedIn = true
    var beforeFetch: suspend () -> Unit = {}
    /** Awaited after a fetch has read the server's set, before it answers —
     *  holds a response "on the wire" while the set changes underneath it. */
    var beforeFetchResponse: suspend () -> Unit = {}
    /** Awaited once a hide/unhide/clear has been APPLIED, before it answers. */
    var beforeWriteResponse: suspend () -> Unit = {}

    /** The validator the server would hand out for [country]'s current set:
     *  derived from the content, as `UserStateController`'s strong ETag is. */
    fun etagOf(country: String): String = "\"etag-$country-${(remote[country] ?: emptySet()).sorted().hashCode()}\""

    private fun state(country: String) = HiddenFilmsState(remote[country] ?: emptySet(), etagOf(country), "lm-$country")

    override suspend fun fetch(country: String, etag: String?, lastModified: String?): HiddenFilmsFetchResult {
        fetchCalls += country
        beforeFetch()
        if (shouldFailFetch) throw IOException("no network")
        if (!signedIn) throw IOException("HTTP 401")
        val answer = if (etag != null && etag == etagOf(country)) HiddenFilmsFetchResult.NotModified
            else HiddenFilmsFetchResult.Changed(state(country))
        beforeFetchResponse()
        return answer
    }

    override suspend fun hide(country: String, title: String): HiddenFilmsState {
        hideCalls += title to country
        if (shouldFailWrite) throw IOException("no network")
        if (!signedIn) throw IOException("HTTP 401")
        remote[country] = (remote[country] ?: emptySet()) + title
        return state(country).also { beforeWriteResponse() }
    }

    override suspend fun unhide(country: String, title: String): HiddenFilmsState {
        unhideCalls += title to country
        if (shouldFailWrite) throw IOException("no network")
        if (!signedIn) throw IOException("HTTP 401")
        remote[country] = (remote[country] ?: emptySet()) - title
        return state(country).also { beforeWriteResponse() }
    }

    override suspend fun clear(country: String): HiddenFilmsState {
        clearCalls += country
        if (shouldFailWrite) throw IOException("no network")
        if (!signedIn) throw IOException("HTTP 401")
        remote[country] = emptySet()
        return state(country).also { beforeWriteResponse() }
    }
}

/** The account's stored pick behind a fake `/api/me/state`: a successful
 *  push updates [remote], as the server does. Mirrors iOS `FakeLanguageClient`. */
internal class FakeLanguageClient : LanguageClient {
    var remote: String? = null
    var shouldFailPush = false
    /** Refuse every push for good, as the server answers a language it does
     *  not know (400). */
    var refusePush = false
    var shouldFailFetch = false
    /** As [FakeHiddenFilmsClient.signedIn]. */
    var signedIn = true
    /** Every push that SUCCEEDED, in order. */
    val pushes = mutableListOf<String>()
    var lastPushed: String? = null
    var pushCount = 0
    /** Every push attempt, failed or not. */
    var pushAttempts = 0

    /** Awaited once a push has REACHED the server, before its response —
     *  holds it "in flight". Cancelling the caller then can't un-send it,
     *  as with the real blocking OkHttp call; throwing from it models a
     *  response lost after the server applied the push. */
    var beforePushResponse: suspend () -> Unit = {}
    /** Awaited at the start of a fetch — holds it "in flight". */
    var beforeFetch: suspend () -> Unit = {}

    override suspend fun fetch(): String? {
        beforeFetch()
        if (shouldFailFetch) throw IOException("no network")
        if (!signedIn) throw IOException("HTTP 401")
        return remote
    }
    private var pushesInFlight = 0
    /** The most pushes ever on the wire at once. */
    var maxPushesInFlight = 0
        private set

    override suspend fun push(language: String) {
        pushAttempts++
        if (shouldFailPush) throw IOException("HTTP 503")
        if (refusePush) throw LanguagePushRefused(400)
        if (!signedIn) throw IOException("HTTP 401")
        remote = language
        pushesInFlight++
        maxPushesInFlight = maxOf(maxPushesInFlight, pushesInFlight)
        // Once sent, cancelling the caller can't take the request back —
        // as with the real blocking OkHttp call.
        try { withContext(NonCancellable) { beforePushResponse() } } finally { pushesInFlight-- }
        pushes += language
        lastPushed = language
        pushCount++
    }
}
