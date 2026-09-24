package pl.kinowo.order

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import java.lang.reflect.Modifier

/** What [ShuffledOrderSuite] runs: every test class, nested ones too — a
 *  nested class skipped by the shuffle is never met in a new order. */
class ShuffledOrderTest {

    private val classes = ShuffledOrder.testClasses(ShuffledOrderSuite::class.java).toList()

    @Test
    fun nestedTestClassesAreIncluded() {
        assertTrue(NestedFixture::class.java in classes)
        assertTrue(ShuffledOrderTest::class.java in classes)
    }

    @Test
    fun classesJUnitCannotInstantiateAreLeftOut() {
        assertFalse(classes.any { it.isAnonymousClass || it.isLocalClass || it.isSynthetic || it.isMemberClass && !Modifier.isStatic(it.modifiers) })
    }

    /** A nested test class — Gradle runs it; the shuffled suite must too. */
    class NestedFixture {
        @Test fun runs() = Unit
    }
}
