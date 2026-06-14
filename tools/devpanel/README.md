# DevPanel

A small always-on-top macOS palette that floats over your desktop with four
buttons for the movies repo. Each button opens a Terminal window running the
matching script, so you watch the sbt/gradle/xcodebuild output and Ctrl-C
stops it.

| Button         | Runs                          | What it does                                   |
|----------------|-------------------------------|------------------------------------------------|
| Android → device | `cd android && ./gradlew runOnDevice` | builds the signed releaseFast APK, installs + launches it on the cabled phone |
| iOS → device     | `xcodebuild … build install`          | resolves the cabled iPhone's UDID, builds + installs the `Kinowo` scheme |
| Web server       | `sbt web/run`                         | local Play app on :9000 |
| Web + worker     | `sbt localStack`                      | web on :9000 + a fixture-replaying worker (local Mongo :27018) |

## Build & run

```bash
bash tools/devpanel/build.sh
```

This compiles `build/DevPanel.app` and opens it (top-left of the screen). It has
no Dock icon (`LSUIElement`); quit with the `✕` in the panel. Drag the panel by
its background to reposition. The panel stays above other windows and follows
you across Spaces.

The absolute path to `scripts/` is baked into the app's Info.plist at build
time, so the `.app` works even if copied to `/Applications`. **Re-run
`build.sh` if you move the repo.**

## Notes

- **iOS** needs the device paired/trusted and your signing team set on the
  `Kinowo` target in Xcode once; the script passes `-allowProvisioningUpdates`
  but can't invent a team. If no device is found it prints the connected list
  and exits.
- **Android** needs release signing creds (`keystore.properties` or
  `KINOWO_RELEASE_*`), same as `runOnDevice` from the shell.

## Tests

```bash
bash tools/devpanel/test.sh
```

Asserts each script dispatches the exact intended command (`DEVPANEL_PRINT_ONLY=1`),
bash-syntax-checks the scripts, and type-checks the Swift.
