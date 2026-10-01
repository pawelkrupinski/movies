package pl.kinowo

import org.junit.Assert.assertFalse
import org.junit.Test

/**
 * The app generates no `BuildConfig`. AGP writes the variant's versionCode into
 * it, and CI builds the release with a fresh versionCode every run (the run
 * number, see app/build.gradle.kts) — so a `BuildConfig` made both release
 * variants' Kotlin compile and Compose-mapping inputs differ from the last run's,
 * and CI recompiled them instead of restoring them from the Gradle build cache.
 * The tuning switch it used to carry is [TUNING_ENABLED], from a per-build-type
 * source set.
 */
class NoBuildConfigTest {

    @Test
    fun theBuildGeneratesNoBuildConfigClass() {
        val found = runCatching { Class.forName("pl.kinowo.BuildConfig") }.isSuccess
        assertFalse("buildFeatures.buildConfig must stay off — it carries the per-run versionCode", found)
    }
}
