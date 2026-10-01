package pl.kinowo

/**
 * Whether this build ships the non-prod tweak screen. `false` here, in the
 * `src/noTuning` source set (the public `release` / `releaseFast`); `src/tuning`
 * holds the `true` copy. A compile-time constant, so the screen is compiled
 * out of these builds.
 */
const val TUNING_ENABLED = false
