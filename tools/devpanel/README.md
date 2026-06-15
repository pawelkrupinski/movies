# DevPanel

A small always-on-top macOS palette that floats over your desktop with six
buttons for the movies repo. Every action streams its live output into an
in-panel console.

| Button         | Runs                          | Console |
|----------------|-------------------------------|---------|
| Android → device | wait-for-unlock → `cd android && ./gradlew runOnDevice` | Device |
| iOS → device     | wait-for-unlock → `xcodebuild build` → `devicectl install` → `devicectl launch` | Device |
| Web server       | frees :9000 + reaps a stale fixture worker → `sbt web/run`            | Web |
| Web + worker     | frees :9000 + reaps a stale fixture worker → `sbt localStack` (web + fixture worker, local Mongo :27018) | Web |
| Kill web + worker | stops the running web action (SIGTERMs its process group) then frees :9000 + reaps the forked fixture worker | Web |
| Reset local corpus | `scripts/reset-corpus.sh --local --yes` (drops the kinowo_local corpus collections so a local worker re-scrapes) | Web |

## Consoles

Two collapsible consoles, each with its own **Stop** button:

- **Web** — shared by the two server actions; **keeps** its scrollback across
  runs (long-lived servers you want to keep watching). Its **Stop** also reaps
  a stale fixture worker — the web+worker stack forks the worker JVM
  (`LocalFixtureWorkerMain`) into its own process tree, so killing the script's
  process group alone leaves it running and still projecting fixtures into the
  local Mongo.
- **Device** — shared by Android + iOS; **cleared** at the start of each run.

Toggle either with its disclosure triangle. When **both** are collapsed the
panel shrinks to a fixed narrow size that just fits the button labels; when one
is open the panel is **resizable by hand** (drag any edge) and remembers the
width you set. Log text is selectable and **⌘C** copies it (⌘A selects all).

## Run on a worktree

**Long-press** (or **right-click**) any button to pick which git worktree to
run the task in. The chosen path is handed to the script via
`DEVPANEL_REPO_ROOT`; a plain click runs against the main checkout.

## Wait for phone unlock

Both device actions wait for the phone to be ready before launching:

- **Android** — `wait_for_android_unlock` polls `dumpsys window`'s keyguard
  flag. (If the device exposes no recognised flag — e.g. set to never lock in
  developer options — it doesn't block.)
- **iOS** — two gates, both verified against a real device:
  - `wait_for_ios_unlock` blocks until the iPhone has been unlocked since boot
    (`devicectl … lockState`'s `unlockedSinceBoot`) — the one state that blocks
    `devicectl install`.
  - `devicectl install` actually succeeds on a *screen-locked* iPhone, but
    `devicectl process launch` fails with `Reason: Locked … device was not, or
    could not be, unlocked`. The launch step is wrapped in a retry that waits
    out exactly that error, so the app comes up the moment you unlock.

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
  but can't invent a team. The bundle id is read from the freshly built `.app`,
  so a team-namespace change to it doesn't break the launch step.
- **Android** needs release signing creds (`keystore.properties` or
  `KINOWO_RELEASE_*`), same as `runOnDevice` from the shell.

## Tests

```bash
bash tools/devpanel/test.sh
```

Asserts each script dispatches the intended command(s) (`DEVPANEL_PRINT_ONLY=1`)
including the unlock + worktree-override steps, checks the iOS lock-error
classifier against the real captured failure text and the `unlockedSinceBoot`
parser, checks `free_port` frees a port, bash-syntax-checks the scripts, then
compiles the Swift app and runs its headless self-test — which drives the real
`CommandRunner` output-streaming + `DEVPANEL_REPO_ROOT` env path.
