# App-association files (Universal Links / App Links)

Served by `controllers.WellKnownController` at:

- `/.well-known/apple-app-site-association` — iOS Universal Links
- `/.well-known/assetlinks.json` — Android App Links

These let `https://<host>/...` links (including the copy-to-clipboard filter
links) open the native apps instead of the browser. The same web binary +
resources deploy to every country, so this identical AASA is served on all three
deployments — `kinowo.fly.dev` (PL), `showtimes-uk.fly.dev` (UK),
`showtimes-de.fly.dev` (DE) — and one app ID (`CQ4YC43YDM.dev.kinowo.Kinowo`)
claims links on all of them.

## iOS Universal Links need a PAID Apple Developer account

The AASA above is served, and iOS Universal Links also require the app to carry
the **Associated Domains** entitlement — which a **free/personal** Apple team
cannot provision ("Personal development teams do not support the Associated
Domains capability"). On the paid team this is now wired in:
`ios/Kinowo/Kinowo.entitlements` lists `applinks:` for all three hosts and is
referenced by `CODE_SIGN_ENTITLEMENTS` in both app-target build configs. With
automatic signing Xcode enables the capability for the App ID on the next
build/archive. (The `kinowo://` custom URL scheme still works as a fallback and
needs no paid account.) Android App Links have no such gate.

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
