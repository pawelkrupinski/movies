package pl.kinowo

/**
 * Whether this build ships the non-prod tweak screen. `true` here, in the
 * `src/tuning` source set (`debug`, `tuneRelease`); `src/noTuning` holds the
 * `false` copy for the public `release` / `releaseFast`. A compile-time
 * constant, so the screen is compiled out where it is `false`.
 */
const val TUNING_ENABLED = true
