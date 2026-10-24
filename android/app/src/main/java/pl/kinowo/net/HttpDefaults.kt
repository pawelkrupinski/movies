package pl.kinowo.net

import okhttp3.MediaType.Companion.toMediaType

/**
 * What every request this app makes identifies itself as:
 * `KinowoAndroid/<versionName> (Android <release>; API <sdk>)`, e.g. `KinowoAndroid/2.1.3 (Android 15; API 35)`.
 * The server sorts a call to the app by its `KinowoAndroid/` prefix (`WebRouteUsageMetrics.clientKind`); the
 * version and OS behind it say which builds and Android releases still call a route. Built once, at the
 * composition root ([pl.kinowo.kinowoViewModelFactory]), from the installed versionName and `Build.VERSION`;
 * `0` stands in for a versionName that could not be read.
 */
fun appUserAgent(versionName: String, androidRelease: String?, sdkInt: Int): String {
    val version = versionName.ifEmpty { "0" }
    val release = androidRelease?.ifEmpty { null } ?: "unknown"
    return "KinowoAndroid/$version (Android $release; API $sdkInt)"
}

/** The body type of every JSON request this app sends. */
val JSON_MEDIA_TYPE = "application/json".toMediaType()
