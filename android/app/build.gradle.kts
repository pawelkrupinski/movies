import java.io.File
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

    // Release signing is wired only when credentials are present — from CI env
    // vars (the workflow decodes the base64 keystore secret to a file and exports
    // the three passwords) or a local, gitignored `keystore.properties`. With
    // neither, `release` stays unsigned so `assembleRelease` still builds locally
    // for a smoke test; only signed builds are uploadable to Play.
    val keystoreProps = Properties().apply {
        val f = rootProject.file("keystore.properties")
        if (f.exists()) f.inputStream().use { load(it) }
    }
    fun signingValue(envName: String, propName: String): String? =
        (System.getenv(envName) ?: keystoreProps.getProperty(propName))?.takeIf { it.isNotBlank() }
    val releaseStorePath = signingValue("KINOWO_RELEASE_STORE_FILE", "storeFile")
        ?.let { rootProject.file(it) }
    val hasReleaseSigning = releaseStorePath?.exists() == true

    signingConfigs {
        if (hasReleaseSigning) {
            create("release") {
                storeFile = releaseStorePath
                storePassword = signingValue("KINOWO_RELEASE_STORE_PASSWORD", "storePassword")
                keyAlias = signingValue("KINOWO_RELEASE_KEY_ALIAS", "keyAlias")
                keyPassword = signingValue("KINOWO_RELEASE_KEY_PASSWORD", "keyPassword")
            }
        }
    }

    buildTypes {
        release {
            isMinifyEnabled = true
            isShrinkResources = true
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
            if (hasReleaseSigning) {
                signingConfig = signingConfigs.getByName("release")
            } else {
                logger.warn("kinowo: no release signing credentials — `release` build will be UNSIGNED (not uploadable to Play).")
            }
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
        unitTests {
            isReturnDefaultValues = true
            // Robolectric needs the merged resources/manifest on the JVM
            // classpath to render real Compose layouts off-device.
            isIncludeAndroidResources = true
        }
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

    // Custom Tabs for the web OAuth sign-in flow (the Android analog of iOS's
    // ASWebAuthenticationSession): an in-app browser tab that shares no cookies
    // with the app, so sign-in completes via the kinowo:// deep-link + one-shot
    // exchange code.
    implementation("androidx.browser:browser:1.8.0")

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

    // JVM (off-device) Compose UI tests via Robolectric — renders the real
    // composables and measures layout bounds without an emulator, so the
    // "two showtime chips fit one row" guarantee runs in CI. ui-test-manifest
    // (already a debugImplementation) supplies the ComponentActivity that
    // createComposeRule launches.
    testImplementation(composeBom)
    testImplementation("org.robolectric:robolectric:4.13")
    testImplementation("androidx.compose.ui:ui-test-junit4")

    androidTestImplementation("androidx.test.ext:junit:1.2.1")
    androidTestImplementation("androidx.test.espresso:espresso-core:3.6.1")
    androidTestImplementation("androidx.compose.ui:ui-test-junit4")
    debugImplementation("androidx.compose.ui:ui-tooling")
    debugImplementation("androidx.compose.ui:ui-test-manifest")
}

// ── Emulator helpers ────────────────────────────────────────────────────────
// `./gradlew runOnEmulator`   — boot an AVD if needed, install, launch, stream
//                               the app's logcat, AND keep watching src/main:
//                               whenever this terminal is focused and sources
//                               changed, rebuild + redeploy (only if it
//                               compiles). Ctrl+C to stop.
// `./gradlew debugOnEmulator` — boot/install/launch waiting for a debugger and
//                               wire up JDWP so an IDE or jdb can attach.
// `./gradlew bootEmulator`    — just the boot half.
// Pick a different AVD with `-Pavd=<name>` (default `kinowo`); change the JDWP
// port with `-PdebugPort=<n>` (default 5005). The auto-redeploy only fires
// while the launching terminal is frontmost — override the match that decides
// "is the terminal focused" with `-PfocusApp=<AppName>` (comma-separated).
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
    description = "Boot/install/launch, stream logcat, and auto-rebuild + redeploy on source changes while this terminal is focused. Pass -Ptuning to launch into the showtime tuning screen."
    dependsOn("bootEmulator", "installDebug")
    val adb = adbExe
    val appPkg = appId
    val component = mainComponent
    val noSdk = noSdkMessage
    // Captured at configuration time so the action stays config-cache clean.
    val gradlew = rootProject.file("gradlew").absolutePath
    val rootDirPath = rootProject.projectDir.absolutePath
    val watchDir = layout.projectDirectory.dir("src/main").asFile.absolutePath
    val focusOverride = (findProperty("focusApp") as String?)
    // `-Ptuning` launches the non-prod showtime tuning screen via the
    // `kinowo_tuning` intent extra (DEBUG-gated in MainActivity).
    val launchTuning = project.hasProperty("tuning")
    val termProgram: String? = System.getenv("TERM_PROGRAM")
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

        // ── logcat tail, restartable across redeploys ────────────────────────
        // Each (re)launch kills the previous tail and starts a fresh
        // `logcat --pid=<pid>` in a daemon thread. pid is re-resolved every
        // launch because a reinstall hands the app a new pid. Clearing the
        // buffer first scopes the stream to the current run.
        var logcatProc: Process? = null
        fun pidOf(): String {
            var pid = ""
            val deadline = System.currentTimeMillis() + 20_000
            while (pid.isEmpty() && System.currentTimeMillis() < deadline) {
                pid = sh(adb, "-s", serial, "shell", "pidof", appPkg).split(Regex("\\s+")).firstOrNull().orEmpty()
                if (pid.isEmpty()) Thread.sleep(500)
            }
            return pid
        }
        fun launchApp(forceStop: Boolean) {
            if (forceStop) sh(adb, "-s", serial, "shell", "am", "force-stop", appPkg)
            logcatProc?.destroy()
            sh(adb, "-s", serial, "logcat", "-c")
            val startArgs = mutableListOf(adb, "-s", serial, "shell", "am", "start", "-n", component)
            if (launchTuning) startArgs += listOf("--ez", "kinowo_tuning", "true")
            sh(*startArgs.toTypedArray())
            val pid = pidOf()
            if (pid.isEmpty()) {
                logger.lifecycle("⚠  $appPkg isn't running (crash on launch?) — fix it and save, I'll rebuild.")
                logcatProc = null
                return
            }
            val proc = ProcessBuilder(adb, "-s", serial, "logcat", "--pid=$pid").redirectErrorStream(true).start()
            logcatProc = proc
            Thread { proc.inputStream.bufferedReader().forEachLine { logger.lifecycle(it) } }
                .also { it.isDaemon = true; it.start() }
        }

        // ── focus detection (macOS) ──────────────────────────────────────────
        // Only rebuild while THIS terminal is frontmost, so saving in the
        // editor never triggers a build until you switch back here to look.
        // The frontmost app's display name comes from `lsappinfo` (no
        // Automation/TCC prompt, unlike AppleScript). Match it against terminal
        // names seeded from $TERM_PROGRAM, plus whatever app is frontmost right
        // now (this terminal, since you just launched) so it works for any
        // terminal without config. Override with `-PfocusApp=Name1,Name2`.
        val focusTokens = mutableListOf<String>()
        if (!focusOverride.isNullOrBlank()) {
            focusOverride.split(",").mapTo(focusTokens) { it.trim().lowercase() }
        } else {
            focusTokens.addAll(listOf("terminal", "iterm", "ghostty", "warp", "hyper", "alacritty", "kitty", "wezterm", "tabby"))
            if (termProgram == "vscode") focusTokens.add("code") // VS Code's integrated terminal reports "Code"
        }
        focusTokens.removeAll { it.isBlank() }
        fun frontmostApp(): String {
            val asn = sh("lsappinfo", "front").trim().trim('"')
            if (asn.isEmpty()) return ""
            val info = sh("lsappinfo", "info", "-only", "name", asn)
            return Regex("\"LSDisplayName\"=\"(.*)\"").find(info)?.groupValues?.get(1) ?: ""
        }
        fun terminalFocused(): Boolean {
            val front = frontmostApp().lowercase()
            return front.isNotEmpty() && focusTokens.any { front.contains(it) }
        }

        // ── source snapshot + rebuild ────────────────────────────────────────
        fun newestSrcMtime(): Long {
            var m = 0L
            File(watchDir).walkTopDown().forEach { if (it.isFile) m = maxOf(m, it.lastModified()) }
            return m
        }
        // Child Gradle invocation (separate daemon — this build is still alive
        // in the watch loop). `-q` keeps a green build silent; compile errors
        // and BUILD FAILED still print.
        fun rebuild(): Boolean {
            val p = ProcessBuilder(gradlew, ":app:installDebug", "-q")
                .directory(File(rootDirPath))
                .redirectErrorStream(true)
                .start()
            p.inputStream.bufferedReader().forEachLine { logger.lifecycle(it) }
            return p.waitFor() == 0
        }

        // ── initial launch ───────────────────────────────────────────────────
        logger.lifecycle("Launching $component on $serial…")
        launchApp(forceStop = false)

        // Seed the focus match with the current frontmost app (this terminal),
        // unless it's the emulator window or already covered.
        val startupFront = frontmostApp()
        if (focusOverride.isNullOrBlank() && startupFront.isNotBlank()) {
            val f = startupFront.lowercase()
            if (!f.contains("qemu") && !f.contains("emulator") && focusTokens.none { f.contains(it) }) {
                focusTokens.add(f)
            }
        }

        logger.lifecycle(
            "\n👀 Watching src/main — I'll rebuild + redeploy when this terminal is focused and sources changed."
        )
        logger.lifecycle("   Focus match: ${focusTokens.joinToString("/")}  (frontmost now: \"$startupFront\")")
        logger.lifecycle("   Not detecting your terminal? Re-run with -PfocusApp=<AppName>. Ctrl+C to stop.\n")

        var lastBuilt = newestSrcMtime()
        val quietMs = 250L // let a burst of saves settle before building
        while (true) {
            Thread.sleep(200)
            // Cheap local-FS scan first; only pay for the focus check (two
            // lsappinfo spawns) once there's actually a change waiting.
            val newest = newestSrcMtime()
            if (newest <= lastBuilt || System.currentTimeMillis() - newest < quietMs) continue
            if (!terminalFocused()) continue
            logger.lifecycle("↻  Change detected — rebuilding & reinstalling…")
            val ok = rebuild()
            lastBuilt = newestSrcMtime()
            if (ok) {
                launchApp(forceStop = true)
                logger.lifecycle("✓  Redeployed.\n")
            } else {
                logger.lifecycle("✗  Build failed — left the running build in place. Fix & save to retry.\n")
            }
        }
    }
}

tasks.register("debugOnEmulator") {
    group = "emulator"
    description = "Boot/install as runOnEmulator, but launch waiting for a debugger and forward JDWP (port -PdebugPort=, default 5005). Pass -Ptuning to launch into the showtime tuning screen."
    dependsOn("bootEmulator", "installDebug")
    val adb = adbExe
    val appPkg = appId
    val component = mainComponent
    val port = debugJdwpPort
    val sourcePath = layout.projectDirectory.dir("src/main/java").asFile.absolutePath
    val noSdk = noSdkMessage
    // `-Ptuning` launches the non-prod showtime tuning screen via the
    // `kinowo_tuning` intent extra (DEBUG-gated in MainActivity).
    val launchTuning = project.hasProperty("tuning")
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
        val startArgs = mutableListOf(adb, "-s", serial, "shell", "am", "start", "-D", "-n", component)
        if (launchTuning) startArgs += listOf("--ez", "kinowo_tuning", "true")
        sh(*startArgs.toTypedArray())

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
