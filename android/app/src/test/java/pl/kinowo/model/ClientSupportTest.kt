package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.recordedFixture

/** The Kotlin mirror of Scala `ClientVersion` / `ClientSupport` — same cases. */
class ClientSupportTest {

    private val gated = ClientSupport.Platform(minimumVersion = "1.2", storeUrl = "https://play.example/app")

    @Test
    fun parsesDottedNumericVersions() {
        assertEquals(listOf(1, 2, 3), ClientVersion.parse("1.2.3"))
        assertEquals(listOf(1, 0), ClientVersion.parse(" 1.0 "))
        assertNull(ClientVersion.parse("1.2-beta"))
        assertNull(ClientVersion.parse("1..2"))
        assertNull(ClientVersion.parse(""))
        assertNull(ClientVersion.parse("v1"))
    }

    @Test
    fun comparesComponentWiseNotAsStrings() {
        assertEquals(true, ClientVersion.isAtLeast("1.10", "1.9"))
        assertEquals(false, ClientVersion.isAtLeast("1.9", "1.10"))
        assertEquals(true, ClientVersion.isAtLeast("1.0", "1.0.0"))
        assertEquals(true, ClientVersion.isAtLeast("1.0.0", "1.0"))
        assertEquals(false, ClientVersion.isAtLeast("1.0", "1.0.1"))
        assertEquals(true, ClientVersion.isAtLeast("2", "1.99.99"))
        assertNull(ClientVersion.isAtLeast("dev", "1.0"))
        assertNull(ClientVersion.isAtLeast("1.0", "x"))
    }

    @Test
    fun unsupportedOnlyWhenGatedParsedAndBelow() {
        assertFalse(ClientVersion.isSupported("1.1.9", gated))
        assertTrue(ClientVersion.isSupported("1.2", gated))
        assertTrue(ClientVersion.isSupported("1.2.0", gated))
        assertTrue(ClientVersion.isSupported("1.10", gated))
    }

    @Test
    fun anythingUnknownIsSupported() {
        assertTrue("unparseable client", ClientVersion.isSupported("dev-build", gated))
        assertTrue("unparseable minimum", ClientVersion.isSupported("1.0", gated.copy(minimumVersion = "soon")))
        assertTrue("no store url", ClientVersion.isSupported("0.1", gated.copy(storeUrl = null)))
        assertTrue("no minimum", ClientVersion.isSupported("0.1", gated.copy(minimumVersion = null)))
        assertTrue("no platform", ClientVersion.isSupported("0.1", null))
    }

    @Test
    fun decodesTheLivePayload() {
        val support = ClientSupport.parse(recordedFixture("client_support.json"))!!
        assertEquals(
            ClientSupport.Platform("1.0", "https://play.google.com/store/apps/details?id=net.pawel.kinowo"),
            support.android,
        )
        assertTrue(support.android!!.isGated)
        assertEquals("https://apps.apple.com/app/id6792566321", support.ios?.storeUrl)
    }

    @Test
    fun decodesNullsAsUngated() {
        val support = ClientSupport.parse("""{"android":{"minimumVersion":null,"storeUrl":null},"ios":null}""")!!
        assertFalse(support.android!!.isGated)
        assertNull(support.ios)
    }

    @Test
    fun aBodyThatIsNotThePayloadParsesToNull() {
        assertNull(ClientSupport.parse("<html>502</html>"))
        assertNull(ClientSupport.parse("""{"android":{"minimumVersion":3}}"""))
    }
}
