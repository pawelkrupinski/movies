package pl.kinowo

import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * The debug build's manifest declares the "Kinowo Tune" launcher
 * (`pl.kinowo.TuningLauncherActivity`, from `src/tuning`), so its class must
 * be compiled into the same variant. Under AGP's built-in Kotlin a plain
 * `java.srcDir` no longer feeds `.kt` files to the compiler, which left the
 * manifest entry pointing at a class the APK didn't contain — the icon
 * crashed with ClassNotFoundException on tap. Unit tests run against the
 * debug variant's classes, so a lookup here fails exactly when the APK would.
 */
class TuningLauncherCompiledTest {

    @Test
    fun theTuningLauncherIsCompiledIntoTheDebugVariant() {
        val found = runCatching { Class.forName("pl.kinowo.TuningLauncherActivity") }.isSuccess
        assertTrue("src/tuning must be compiled into debug, whose manifest declares its activity", found)
    }
}
