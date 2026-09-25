package pl.kinowo.net

import okhttp3.MediaType.Companion.toMediaType

/** What every request this app makes identifies itself as. */
const val USER_AGENT = "KinowoAndroid/1.0"

/** The body type of every JSON request this app sends. */
val JSON_MEDIA_TYPE = "application/json".toMediaType()
