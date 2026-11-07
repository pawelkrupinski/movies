pluginManagement {
    repositories {
        google {
            content {
                includeGroupByRegex("com\\.android.*")
                includeGroupByRegex("com\\.google.*")
                includeGroupByRegex("androidx.*")
            }
        }
        mavenCentral()
        gradlePluginPortal()
    }
}
dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        google()
        // Robolectric's ~150 MB android-all jar only from Google's mirror of Maven Central:
        // Maven Central answers the shared GitHub runner addresses 429 for it (Android #430,
        // Order independence #51 and #52 -- the last through Gradle itself, so Gradle's cache
        // alone did not save a cold run).
        exclusiveContent {
            forRepository {
                maven("https://maven-central.storage-download.googleapis.com/maven2/") {
                    name = "MavenCentralGoogleMirror"
                }
            }
            filter { includeModule("org.robolectric", "android-all-instrumented") }
        }
        mavenCentral()
    }
}

rootProject.name = "Kinowo"
include(":app")
include(":baselineprofile")
