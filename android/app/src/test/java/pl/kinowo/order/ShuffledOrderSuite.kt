package pl.kinowo.order

import org.junit.Test
import org.junit.runner.Description
import org.junit.runner.RunWith
import org.junit.runner.manipulation.Sorter
import org.junit.runners.Suite
import org.junit.runners.model.RunnerBuilder
import java.io.File
import java.lang.reflect.Modifier

/**
 * Every unit test in this module, in an order drawn from a seed.
 *
 * Gradle runs the Robolectric suite in ONE JVM, in the order it happens to find the class
 * files, and JUnit 4 runs a class's methods in a fixed hash order — so a test that only
 * passes because another one ran first (or never ran first) looks healthy forever. The
 * prefs DataStore is a process-wide singleton, and "null until set" assertions passed or
 * failed by which test wrote first (fixed in 40ffc4b55 with FreshUserPreferences). This
 * suite reorders both the classes and the methods inside each class by a seed, so the
 * nightly order-independence job meets a different order every night, and a failure is
 * reproducible from the seed it prints:
 *
 *     ./gradlew testDebugUnitTest -PtestOrderSeed=<seed>
 *
 * Without `testOrderSeed` the build excludes this class, so the ordinary run is unchanged.
 */
@RunWith(ShuffledOrder::class)
class ShuffledOrderSuite

class ShuffledOrder(suite: Class<*>, builder: RunnerBuilder) : Suite(builder, suite, testClasses(suite)) {

    init {
        val seed = System.getProperty(SEED_PROPERTY)?.toLongOrNull()
            ?: error("ShuffledOrderSuite needs -D$SEED_PROPERTY (run with -PtestOrderSeed=<n>)")
        println("ShuffledOrderSuite: seed $seed — reproduce with ./gradlew testDebugUnitTest -PtestOrderSeed=$seed")
        sort(Sorter(compareBy<Description> { mix(seed, it.displayName) }.thenBy { it.displayName }))
    }

    companion object {
        const val SEED_PROPERTY = "kinowo.testOrderSeed"

        /** A stable, well-spread position for `name` under `seed` (SplitMix64 over the name's hash). */
        fun mix(seed: Long, name: String): Long {
            var z = seed + name.hashCode().toLong() * -0x61c8864680b583ebL
            z = (z xor (z ushr 30)) * -0x40a7b892e31b1a47L
            z = (z xor (z ushr 27)) * -0x6b2fb644ecceee15L
            return z xor (z ushr 31)
        }

        /** Every concrete class compiled beside [suite] with a JUnit 4 `@Test`
         *  method, nested static classes included. */
        fun testClasses(suite: Class<*>): Array<Class<*>> {
            val root = File(suite.protectionDomain.codeSource.location.toURI())
            return root.walkTopDown()
                .filter { it.isFile && it.name.endsWith(".class") }
                .map { it.relativeTo(root).path.removeSuffix(".class").replace(File.separatorChar, '.') }
                .mapNotNull { runCatching { Class.forName(it, false, suite.classLoader) }.getOrNull() }
                .filter { it != suite && !Modifier.isAbstract(it.modifiers) && !it.isInterface }
                // Nested classes too, but only the kind JUnit can instantiate.
                .filter { !it.isAnonymousClass && !it.isLocalClass && !it.isSynthetic }
                .filter { !it.isMemberClass || Modifier.isStatic(it.modifiers) }
                .filter { c -> c.methods.any { it.isAnnotationPresent(Test::class.java) } }
                .sortedBy { it.name }
                .toList()
                .toTypedArray()
        }
    }
}
