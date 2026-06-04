package pl.kinowo

import android.app.Activity
import android.content.Intent
import android.os.Bundle

/**
 * Second launcher icon ("Kinowo Tune"). Tapping it trampolines straight into
 * [MainActivity] with the `kinowo_tuning` extra set, so the non-prod tweak
 * screen can be opened on-device without `adb`.
 *
 * Lives in the `src/tuning` source set, shared by the builds that enable tuning
 * — `debug` and the signed, minified `tuneRelease` — so it shows up in those
 * and never in the public `release` / Play build.
 *
 * `MainActivity` is `singleTask` and reads the tuning flag in `onCreate`, so a
 * plain re-launch of an already-running instance would only deliver
 * `onNewIntent` and stay in normal mode. `CLEAR_TASK` (with `NEW_TASK`) clears
 * the existing task and starts a fresh `MainActivity`, guaranteeing `onCreate`
 * re-reads the extra and shows the tweak screen.
 */
class TuningLauncherActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        startActivity(
            Intent(this, MainActivity::class.java)
                .putExtra("kinowo_tuning", true)
                .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK),
        )
        finish()
    }
}
