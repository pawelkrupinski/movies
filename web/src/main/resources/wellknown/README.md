# App-association files (Universal Links / App Links)

Served by `controllers.WellKnownController` at:

- `/.well-known/apple-app-site-association` — iOS Universal Links
- `/.well-known/assetlinks.json` — Android App Links

These let `https://kinowo.fly.dev/...` links (including the copy-to-clipboard
filter links) open the native apps instead of the browser.

## Android signing fingerprints (`assetlinks.json`)

App Links verify only when the SHA-256 of the cert that signed the **installed**
app appears in `sha256_cert_fingerprints`. Listed today:

- `pl.kinowo` → the **upload/release** keystore (`android/kinowo-release.jks`,
  alias `kinowo`). Covers a sideloaded release APK signed with that key.
- `pl.kinowo.debug` → the local Android **debug** keystore. Covers a locally
  installed debug build.

### ⚠️ Still required for Play Store installs

When the app is distributed through **Google Play**, Google re-signs it with the
**Play App Signing** key — a *different* cert than the upload key above. Until
that key's SHA-256 is added here, App Links will NOT auto-verify for a
Play-installed build and `https://kinowo.fly.dev/...` links open the browser.

Get it from **Play Console → (app) → Test and release → App integrity → App
signing → "SHA-256 certificate fingerprint"**, then add it to the `pl.kinowo`
entry's `sha256_cert_fingerprints` array (a fingerprint array may hold several —
keep the upload one too). Redeploy web so the file goes live, then reinstall or
run `adb shell pm verify-app-links --re-verify pl.kinowo`.

`kinowo://...` custom-scheme links open the app regardless of any of this — only
the `https` App Links depend on these fingerprints.
