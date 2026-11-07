package pl.kinowo

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Robolectric runs offline over the android-all jar Gradle resolved (app/build.gradle.kts,
 * "Robolectric's android runtime through Gradle"): its own fetch from Maven Central at test
 * time drew a 429 on 2026-11-07 and failed 68 classes in CI.
 */
class RobolectricOfflineTest {
    @Test
    fun robolectricNeverFetchesItsRuntimeAtTestTime() {
        assertEquals("true", System.getProperty("robolectric.offline"))
        val dependencyDirectory = File(System.getProperty("robolectric.dependency.dir") ?: "")
        val jars = dependencyDirectory.listFiles().orEmpty().map { it.name }
        assertTrue(
            "android-all-instrumented in $dependencyDirectory: $jars",
            jars.any { it.startsWith("android-all-instrumented-14-robolectric-") && it.endsWith(".jar") },
        )
    }
}
