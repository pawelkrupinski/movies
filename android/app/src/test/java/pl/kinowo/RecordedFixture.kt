package pl.kinowo

/** A response captured from a live endpoint, under `src/test/resources/fixtures/`. */
fun recordedFixture(name: String): String =
    checkNotNull(RecordedFixtureAnchor::class.java.classLoader!!.getResourceAsStream("fixtures/$name")) { "no fixture $name" }
        .bufferedReader().use { it.readText() }

private object RecordedFixtureAnchor
