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

    // Backdrop blur for the floating search pill — real frosted-glass that
    // distorts the grid scrolling under it (RenderEffect on API 32+, graceful
    // tint-only fallback below). 1.1.x is the Compose 1.7 line; newer Haze
    // needs Compose 1.8+.
    implementation("dev.chrisbanes.haze:haze:1.1.1")

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
// `./gradlew runOnEmulator`   — boot an AVD if needed, install, launch, then
//                               stream the app's logcat until you Ctrl+C.
// `./gradlew debugOnEmulator` — boot/install/launch waiting for a debugger and
//                               wire up JDWP so an IDE or jdb can attach.
// `./gradlew bootEmulator`    — just the boot half.
// Pick a different AVD with `-Pavd=<name>` (default `kinowo`); change the JDWP
// port with `-PdebugPort=<n>` (default 5005).
//
// The adb path / AVD name are resolved here at configuration time and captured
// as plain values; the adb-shelling helpers are inlined into each task action.
// Both keep the actions free of Project/script references, so these tasks stay
// configuration-cache compatible. That forces a small `sh` duplication across
// the actions — the alternative (a shared script-level fun) is exactly what the
// config cache can't serialise.

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
val debugJdwpPort: String = (findProperty("debugPort") as String?) ?: "5005"
val appId = "pl.kinowo"
val mainComponent = "$appId/.MainActivity"
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
    description = "Boot an emulator if needed, install the debug build, launch the app, and stream its logcat."
    dependsOn("bootEmulator", "installDebug")
    val adb = adbExe
    val appPkg = appId
    val component = mainComponent
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

        // Clear the buffer first so we only stream this run's logs; the app's
        // own startup lines stay (they're written after the clear, before we
        // start reading, and logcat replays the buffer before tailing).
        sh(adb, "-s", serial, "logcat", "-c")
        logger.lifecycle("Launching $component on $serial…")
        sh(adb, "-s", serial, "shell", "am", "start", "-n", component)

        // Scope logcat to just this app — resolve its pid (am start returns
        // before the process is necessarily up, so poll).
        var pid = ""
        val deadline = System.currentTimeMillis() + 20_000
        while (pid.isEmpty() && System.currentTimeMillis() < deadline) {
            pid = sh(adb, "-s", serial, "shell", "pidof", appPkg).split(Regex("\\s+")).firstOrNull().orEmpty()
            if (pid.isEmpty()) Thread.sleep(500)
        }
        if (pid.isEmpty()) throw GradleException("Couldn't find a running $appPkg process to tail (did the launch crash?).")

        logger.lifecycle("Streaming logcat for $appPkg (pid $pid) — Ctrl+C to stop.\n")
        val logcat = ProcessBuilder(adb, "-s", serial, "logcat", "--pid=$pid").redirectErrorStream(true).start()
        // Blocks until logcat ends (i.e. you Ctrl+C the build, or the app dies).
        logcat.inputStream.bufferedReader().forEachLine { logger.lifecycle(it) }
        logcat.waitFor()
    }
}

tasks.register("debugOnEmulator") {
    group = "emulator"
    description = "Boot/install as runOnEmulator, but launch waiting for a debugger and forward JDWP (port -PdebugPort=, default 5005)."
    dependsOn("bootEmulator", "installDebug")
    val adb = adbExe
    val appPkg = appId
    val component = mainComponent
    val port = debugJdwpPort
    val sourcePath = layout.projectDirectory.dir("src/main/java").asFile.absolutePath
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

        // `-D` makes the app process spawn and block until a debugger attaches.
        logger.lifecycle("Launching $component on $serial (waiting for debugger)…")
        sh(adb, "-s", serial, "shell", "am", "start", "-D", "-n", component)

        // `am start -D` returns before the process is necessarily up; poll pidof.
        var pid = ""
        val deadline = System.currentTimeMillis() + 20_000
        while (pid.isEmpty() && System.currentTimeMillis() < deadline) {
            pid = sh(adb, "-s", serial, "shell", "pidof", appPkg).split(Regex("\\s+")).firstOrNull().orEmpty()
            if (pid.isEmpty()) Thread.sleep(500)
        }
        if (pid.isEmpty()) throw GradleException("Couldn't find a running $appPkg process to debug (is the launch crashing?).")

        // Map a fixed local TCP port onto the app's JDWP channel so any JDWP
        // client (IDE or jdb) can reach it at localhost:$port.
        sh(adb, "-s", serial, "forward", "tcp:$port", "jdwp:$pid")

        val jdbCmd = "jdb -connect com.sun.jdi.SocketAttach:hostname=localhost,port=$port -sourcepath $sourcePath"
        logger.lifecycle(
            """
            |
            |Debug session ready — $appPkg (pid $pid) is paused waiting for a debugger.
            |JDWP is forwarded to localhost:$port.
            |
            |Attach one of these:
            |  • IntelliJ / Android Studio: Run ▸ Attach Debugger to Android Process ▸ $appPkg
            |        (or a 'Remote JVM Debug' run config → host localhost, port $port, then Debug)
            |  • Terminal (jdb): run this in your own shell (Gradle can't host its prompt):
            |        $jdbCmd
            |
            |The app resumes as soon as a debugger attaches. To free the port later:
            |        ${adb} -s $serial forward --remove tcp:$port
            """.trimMargin()
        )
    }
}
