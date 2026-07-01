// Top-level build file — plugin versions declared here, applied per-module.
plugins {
    // AGP 9 ships built-in Kotlin support, so the standalone
    // `org.jetbrains.kotlin.android` plugin is gone — AGP pulls KGP itself, and
    // the compose/serialization compiler plugins below pin it to 2.4.0.
    id("com.android.application") version "9.2.1" apply false
    id("org.jetbrains.kotlin.plugin.compose") version "2.4.0" apply false
    id("org.jetbrains.kotlin.plugin.serialization") version "2.4.0" apply false
    // Play Console automation (Gradle Play Publisher): upload the AAB, promote
    // tracks, and publish the store listing / description. 4.0.0 is the first
    // GPP release that supports AGP 9 (verified applying cleanly here even
    // though this project drops the standalone Kotlin plugin — the AGP-9.0-alpha
    // `BaseAppModuleExtension` breakage in GPP issue #1185 is gone by AGP 9.2).
    id("com.github.triplet.play") version "4.0.0" apply false
}
