// Top-level build file — plugin versions declared here, applied per-module.
plugins {
    // AGP 9 ships built-in Kotlin support, so the standalone
    // `org.jetbrains.kotlin.android` plugin is gone — AGP pulls KGP itself, and
    // the compose/serialization compiler plugins below pin it to 2.4.0.
    id("com.android.application") version "9.2.1" apply false
    id("org.jetbrains.kotlin.plugin.compose") version "2.4.0" apply false
    id("org.jetbrains.kotlin.plugin.serialization") version "2.4.0" apply false
}
