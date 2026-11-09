import org.jetbrains.kotlin.gradle.dsl.JvmTarget

// The Baseline Profile generator and the startup Macrobenchmark — instrumented
// tests that drive the app's nonMinifiedRelease / benchmarkRelease builds on a
// connected device or emulator, against an on-device fixture server (see
// FixtureBackend), never prod.
//
//   ./gradlew :app:generateBaselineProfile        — regenerate the committed profile
//   ./gradlew :baselineprofile:connectedBenchmarkReleaseAndroidTest \
//       -Pandroid.testInstrumentationRunnerArguments.class=pl.kinowo.baselineprofile.StartupBenchmark
//                                                  — startup with vs without it
plugins {
    id("com.android.test")
    id("androidx.baselineprofile")
}

android {
    namespace = "pl.kinowo.baselineprofile"
    compileSdk = 37

    defaultConfig {
        // Baseline Profile generation needs API 28+ (33+ without root).
        minSdk = 28
        targetSdk = 37
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    targetProjectPath = ":app"

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

kotlin {
    compilerOptions {
        jvmTarget = JvmTarget.JVM_17
    }
}

baselineProfile {
    // Whatever `adb devices` shows (an emulator locally); no managed devices.
    useConnectedDevices = true
}

dependencies {
    implementation("androidx.test.ext:junit:1.3.0")
    implementation("androidx.test.uiautomator:uiautomator:2.4.1")
    implementation("androidx.benchmark:benchmark-macro-junit4:1.5.0")
    implementation("com.squareup.okhttp3:mockwebserver3:5.5.1")
}
