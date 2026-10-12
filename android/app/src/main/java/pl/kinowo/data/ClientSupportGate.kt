package pl.kinowo.data

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.model.ClientSupport
import pl.kinowo.model.ClientVersion
import pl.kinowo.net.ClientSupportApi
import pl.kinowo.runCatchingCancellable

/**
 * Decides, once per launch, whether this build ([appVersion], the app's
 * `versionName`) is still served — asking `/api/client-support` and applying
 * [ClientVersion.isSupported] to its `android` half.
 *
 * [updateStoreUrl] is the store page to send the user to when this version is
 * no longer supported, else null. It starts null and stays null on ANY failure
 * — offline, non-2xx, a body that doesn't parse: our outage must never lock a
 * user out of a working app. Nothing is cached across launches; a stale
 * "unsupported" verdict would outlive the server relaxing it.
 */
class ClientSupportGate(
    private val api: ClientSupportApi,
    private val appVersion: String,
) {
    private val _updateStoreUrl = MutableStateFlow<String?>(null)
    val updateStoreUrl: StateFlow<String?> = _updateStoreUrl.asStateFlow()

    suspend fun check() {
        val android = runCatchingCancellable { ClientSupport.parse(api.fetchClientSupport()) }
            .getOrNull()?.android ?: return
        _updateStoreUrl.value = android.storeUrl.takeUnless { ClientVersion.isSupported(appVersion, android) }
    }
}
