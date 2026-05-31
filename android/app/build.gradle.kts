import java.util.Properties

plugins {
    id("com.android.application")
    id("org.jetbrains.kotlin.android")
    id("org.jetbrains.kotlin.plugin.compose")
    id("org.jetbrains.kotlin.plugin.serialization")
}

android {
    namespace = "pl.kinowo"
    compileSdk = 34

    defaultConfig {
        applicationId = "pl.kinowo"
        minSdk = 26
        targetSdk = 34
        versionCode = 1
        versionName = "1.0"
        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions {
        jvmTarget = "17"
    }
    buildFeatures {
        compose = true
        buildConfig = true
    }
    testOptions {
        unitTests.isReturnDefaultValues = true
    }
    packaging {
        resources.excludes += "/META-INF/{AL2.0,LGPL2.1}"
    }
}

dependencies {
    val composeBom = platform("androidx.compose:compose-bom:2024.09.03")
    implementation(composeBom)
    androidTestImplementation(composeBom)

    implementation("androidx.core:core-ktx:1.13.1")
    implementation("androidx.activity:activity-compose:1.9.2")
    implementation("androidx.lifecycle:lifecycle-runtime-ktx:2.8.6")
    implementation("androidx.lifecycle:lifecycle-viewmodel-compose:2.8.6")
    implementation("androidx.lifecycle:lifecycle-runtime-compose:2.8.6")

    implementation("androidx.compose.ui:ui")
    implementation("androidx.compose.ui:ui-graphics")
    implementation("androidx.compose.ui:ui-tooling-preview")
    implementation("androidx.compose.foundation:foundation")
    implementation("androidx.compose.material3:material3:1.3.0")
    implementation("androidx.compose.material:material-icons-extended")

    implementation("androidx.navigation:navigation-compose:2.8.0")

    implementation("io.coil-kt:coil-compose:2.7.0")

    implementation("com.squareup.okhttp3:okhttp:4.12.0")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.7.3")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-android:1.8.1")

    implementation("androidx.datastore:datastore-preferences:1.1.1")

    testImplementation("junit:junit:4.13.2")
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.8.1")

    androidTestImplementation("androidx.test.ext:junit:1.2.1")
    androidTestImplementation("androidx.test.espresso:espresso-core:3.6.1")
    androidTestImplementation("androidx.compose.ui:ui-test-junit4")
    debugImplementation("androidx.compose.ui:ui-tooling")
    debugImplementation("androidx.compose.ui:ui-test-manifest")
}

// ── Emulator helpers ────────────────────────────────────────────────────────
// `./gradlew runOnEmulator` boots an AVD if none is running, installs the debug
// build, and launches the app. Pick a different AVD with `-Pavd=<name>`
// (default `kinowo`); `./gradlew bootEmulator` just does the boot half.
//
// The adb path / AVD name are resolved here at configuration time and captured
// as plain values; the adb-shelling helpers are inlined into each task action.
// Both keep the actions free of Project/script references, so these tasks stay
// configuration-cache compatible. That forces a small `sh` duplication between
// the two actions — the alternative (a shared script-level fun) is exactly what
// the config cache can't serialise.

// Source build of the adb command line, then `sh(...)` it. `am`/`devices`/etc.
val adbExe: String? = run {
    val props = Properties()
    val lp = rootProject.file("local.properties")
    if (lp.exists()) lp.inputStream().use { props.load(it) }
    val sdk = props.getProperty("sdk.dir")
        ?: System.getenv("ANDROID_HOME")
        ?: System.getenv("ANDROID_SDK_ROOT")
    sdk?.let { "$it/platform-tools/adb" }
}
val emulatorExe: String? = adbExe?.removeSuffix("/platform-tools/adb")?.let { "$it/emulator/emulator" }
val emulatorAvd: String = (findProperty("avd") as String?) ?: "kinowo"
val emulatorLogFile = layout.buildDirectory.file("emulator.log")
val noSdkMessage = "Android SDK not found — set sdk.dir in local.properties or ANDROID_HOME / ANDROID_SDK_ROOT."

tasks.register("bootEmulator") {
    group = "emulator"
    description = "Boot the '$emulatorAvd' AVD (override with -Pavd=<name>) unless an emulator is already running."
    val adb = adbExe
    val emu = emulatorExe
    val avd = emulatorAvd
    val logProvider = emulatorLogFile
    val noSdk = noSdkMessage
    doLast {
        fun sh(vararg args: String): String {
            val p = ProcessBuilder(*args).redirectErrorStream(true).start()
            val out = p.inputStream.bufferedReader().use { it.readText() }
            p.waitFor()
            return out.trim()
        }
        fun bootedSerial(): String? =
            sh(adb!!, "devices").lines().drop(1)
                .filter { it.endsWith("\tdevice") }
                .map { it.substringBefore("\t") }
                .filter { it.startsWith("emulator-") }
                .firstOrNull { sh(adb, "-s", it, "shell", "getprop", "sys.boot_completed") == "1" }

        if (adb == null || emu == null) throw GradleException(noSdk)
        sh(adb, "start-server")

        bootedSerial()?.let {
            logger.lifecycle("✓ Emulator already booted: $it")
            return@doLast
        }

        val avds = sh(emu, "-list-avds").lines().map { it.trim() }.filter { it.isNotEmpty() }
        if (avd !in avds) {
            throw GradleException("AVD '$avd' not found. Available: ${avds.joinToString(", ")}. Override with -Pavd=<name>.")
        }

        val log = logProvider.get().asFile.apply { parentFile.mkdirs() }
        logger.lifecycle("Booting AVD '$avd' (log → $log)…")
        ProcessBuilder(emu, "-avd", avd)
            .redirectOutput(ProcessBuilder.Redirect.appendTo(log))
            .redirectErrorStream(true)
            .start()

        val deadline = System.currentTimeMillis() + 180_000
        var serial: String? = null
        while (serial == null && System.currentTimeMillis() < deadline) {
            Thread.sleep(3_000)
            serial = bootedSerial()
        }
        serial ?: throw GradleException("AVD '$avd' didn't finish booting within 180s (see $log).")
        logger.lifecycle("✓ Emulator booted: $serial")
    }
}

// The device must exist before `installDebug` runs.
tasks.matching { it.name == "installDebug" }.configureEach { mustRunAfter("bootEmulator") }

tasks.register("runOnEmulator") {
    group = "emulator"
    description = "Boot an emulator if needed, install the debug build, and launch the app."
    dependsOn("bootEmulator", "installDebug")
    val adb = adbExe
    val noSdk = noSdkMessage
    doLast {
        fun sh(vararg args: String): String {
            val p = ProcessBuilder(*args).redirectErrorStream(true).start()
            val out = p.inputStream.bufferedReader().use { it.readText() }
            p.waitFor()
            return out.trim()
        }
        if (adb == null) throw GradleException(noSdk)
        val serial = sh(adb, "devices").lines().drop(1)
            .filter { it.endsWith("\tdevice") }
            .map { it.substringBefore("\t") }
            .firstOrNull { it.startsWith("emulator-") }
            ?: throw GradleException("No booted emulator to launch on.")
        logger.lifecycle("Launching pl.kinowo on $serial…")
        logger.lifecycle(sh(adb, "-s", serial, "shell", "am", "start", "-n", "pl.kinowo/.MainActivity"))
    }
}
