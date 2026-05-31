package pl.kinowo.auth

import kotlinx.serialization.Serializable

/**
 * The signed-in user as the server reports it from `/api/me`,
 * `/auth/exchange`, and `/auth/token`. The Android counterpart of iOS
 * `UserProfile` — same four fields, same wire shape.
 */
@Serializable
data class UserProfile(
    val displayName: String? = null,
    val email: String? = null,
    val avatarUrl: String? = null,
    val provider: String,
)
