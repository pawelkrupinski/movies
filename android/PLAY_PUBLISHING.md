# Publishing to Google Play, programmatically

The app ships to Google Play via the [Gradle Play Publisher][gpp] (GPP) plugin —
no clicking around `play.google.com/console`. There is **no public API for the
Console web UI**; GPP drives the supported **Google Play Android Developer API**
under the hood (service-account auth, atomic "edit" transactions), which is the
sanctioned way to upload builds, move tracks, and edit the store listing.

## One-time setup (outside CI)

1. **The app must already exist in the Console** with its **first AAB uploaded
   manually once** — the API updates an existing app, it doesn't create one.
2. **Service account:** in Google Cloud, create a service account, enable the
   *Google Play Android Developer API*, and download its JSON key. In the Play
   Console (*Users & permissions*) invite that service account and grant it
   release permission for `net.pawel.kinowo`.
3. **Store the key:**
   - **CI** — paste the JSON as the `PLAY_SERVICE_ACCOUNT_JSON` GitHub Actions
     secret (already wired: the `android.yml` workflow writes it to a file and
     points `KINOWO_PLAY_CREDENTIALS_FILE` at it).
   - **Locally** — drop the JSON at `android/play-credentials.json`
     (git-ignored). Every `./gradlew publish*` command below picks it up.

## Deploying a build

CI does this on a **manual** run (Actions ▸ *Android* ▸ *Run workflow*): it
builds the signed AAB and uploads it to the **`internal`** track, fully rolled
out. Locally the same thing is:

```bash
KINOWO_VERSION_CODE=$(date +%s) ./gradlew :app:publishReleaseBundle
```

Promote an already-uploaded build to a wider track without rebuilding:

```bash
./gradlew :app:promoteReleaseArtifact -Ptrack=production
```

(`track` values: `internal`, `alpha`, `beta`, `production`.) The R8 mapping file
is uploaded automatically so Play can symbolicate crashes.

## Updating the store listing / description

The listing (title, short & full description, and graphics) lives **as files in
the repo** under `app/src/main/play/`, so edits are reviewable diffs. GPP treats
those files as the source of truth, so **pull the current live listing before
editing** — otherwise you'd publish over it with whatever is (or isn't) on disk.

```bash
# 1. Download the CURRENT live listing into app/src/main/play/
./gradlew :app:bootstrapReleaseListing

# 2. Edit the text files, e.g. app/src/main/play/listings/pl-PL/full-description.txt
#    (Kinowo is Polish-first — expect a pl-PL locale; en-US too if present.)

# 3. Push ONLY the listing (does not touch the app binary or tracks):
./gradlew :app:publishReleaseListing
```

Listing file layout GPP expects (per locale directory):

| File | Limit |
|------|-------|
| `listings/<locale>/title.txt` | 30 chars |
| `listings/<locale>/short-description.txt` | 80 chars |
| `listings/<locale>/full-description.txt` | 4000 chars |
| `listings/<locale>/graphics/…` | icon / feature graphic / screenshots |

Store-listing changes still go through Google's review before they appear live.

**Safety:** `app/src/main/play/` is intentionally empty in the repo until you
bootstrap it — with no metadata files present, `publishReleaseListing` is a
no-op, so a deploy can never blank out the live listing by accident. Only
`release` publishes: the `tuneRelease` / `releaseFast` variants are disabled in
`app/build.gradle.kts` so a stray all-variants `publishBundle` can't push a
tweak/unminified build over the real app.

[gpp]: https://github.com/Triple-T/gradle-play-publisher
