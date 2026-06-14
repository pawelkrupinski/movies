# DevPanel

A small always-on-top macOS palette that floats over your desktop with four
buttons for the movies repo.

| Button         | Runs                          | Where output goes |
|----------------|-------------------------------|-------------------|
| Android → device | `cd android && ./gradlew runOnDevice` | **in-panel** live log — builds the signed releaseFast APK, installs + launches it on the cabled phone |
| iOS → device     | `xcodebuild build` → `devicectl install` → `devicectl launch` | **in-panel** live log — resolves the cabled device's UDID, builds the `Kinowo` scheme, installs and launches it |
| Web server       | `sbt web/run`                         | **Terminal** — local Play app on :9000 (frees :9000 first) |
| Web + worker     | `sbt localStack`                      | **Terminal** — web on :9000 + a fixture-replaying worker (local Mongo :27018), frees :9000 first |

The two **device** actions run as a background subprocess and stream their
output into a collapsible area inside the panel (toggle with the **Output**
disclosure triangle; **Stop** terminates the running build). The two **web**
actions are long-lived servers, so they open a Terminal window instead —
full scrollback, Ctrl-C to stop.

## Build & run

```bash
./runDevPanel.sh          # top-level wrapper
# or
bash tools/devpanel/build.sh
```

This compiles `build/DevPanel.app` and opens it (top-left of the screen). It has
no Dock icon (`LSUIElement`); quit with the `✕` in the panel. Drag the panel by
its background to reposition. The panel stays above other windows and follows
you across Spaces.

The absolute path to `scripts/` is baked into the app's Info.plist at build
time, so the `.app` works even if copied to `/Applications`. **Re-run the build
if you move the repo.**

## Notes

- **iOS** needs the device paired/trusted and your signing team set on the
  `Kinowo` target in Xcode once; the script passes `-allowProvisioningUpdates`
  but can't invent a team. If no device is found it prints the connected list
  and exits. The bundle id is read from the freshly built `.app`, so a
  team-namespace change to it doesn't break the launch step.
- **Android** needs release signing creds (`keystore.properties` or
  `KINOWO_RELEASE_*`), same as `runOnDevice` from the shell.

## Tests

```bash
bash tools/devpanel/test.sh
```

Asserts each script dispatches the intended command(s) (`DEVPANEL_PRINT_ONLY=1`),
checks `free_port` actually frees a port, bash-syntax-checks the scripts, then
compiles the Swift app and runs its headless self-test — which drives the real
`CommandRunner` subprocess-streaming path the device buttons use.
