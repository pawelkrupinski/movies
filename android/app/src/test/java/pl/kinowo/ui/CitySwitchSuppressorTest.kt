package pl.kinowo.ui

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * The post-login skip is a strict one-shot: a sign-in arms exactly one skipped
 * proximity check, and every check after that runs normally — so the user is
 * never re-prompted right after a Google / Facebook login, yet still gets the
 * offer when they genuinely travel to a nearer city later.
 */
class CitySwitchSuppressorTest {

    @Test
    fun `does not skip when no sign-in has started`() {
        assertFalse(CitySwitchSuppressor().consumeShouldSkip())
    }

    @Test
    fun `skips exactly one check after a sign-in starts`() {
        val suppressor = CitySwitchSuppressor()
        suppressor.suppressNextCheck()
        assertTrue("the post-login check is skipped", suppressor.consumeShouldSkip())
        assertFalse("the next check runs normally (one-shot)", suppressor.consumeShouldSkip())
    }
}
