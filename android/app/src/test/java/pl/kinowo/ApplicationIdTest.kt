package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * The Play Console registered this app under the package name
 * `net.pawel.kinowo`; an AAB whose applicationId is anything else is rejected
 * at upload ("Your APK or Android App Bundle needs to have the package name
 * net.pawel.kinowo"). Pin it so a stray rename can't silently break uploads.
 *
 * Note this is the installed *applicationId*, deliberately distinct from the
 * `pl.kinowo` code namespace (this very test class lives in `pl.kinowo`).
 */
class ApplicationIdTest {
    @Test
    fun applicationIdIsThePlayRegisteredPackageName() {
        assertEquals("net.pawel.kinowo", BuildConfig.APPLICATION_ID)
    }
}
