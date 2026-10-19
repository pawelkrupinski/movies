# DevPanel

A small always-on-top macOS palette that floats over your desktop with four
buttons for the movies repo. Each button runs one action of
`scripts/devpanel.py` (stdlib Python, run with the system `/usr/bin/python3`)
and streams its live output into an in-panel console. The actions also run
from a terminal: `python3 tools/devpanel/scripts/devpanel.py deploy-ios`.

| Button         | Runs                          | Console |
|----------------|-------------------------------|---------|
| Android → device | wait-for-unlock → `cd android && ../scripts/android-signed ./gradlew runOnDevice` | Device |
| iOS → device     | wait-for-unlock → `xcodebuild build` → `devicectl install` → `devicectl launch` | Device |
| Web server       | frees :9000 → `scripts/dev-server.sh` (`sbt web/run` on a narrowed heap) | Web |
| Kill web         | stops the running web action (SIGTERMs its process group) then frees :9000 | Web |

## Consoles

Two collapsible consoles, each with its own **Stop** button:

- **Web** — shared by the web actions; **keeps** its scrollback across
  runs (long-lived servers you want to keep watching).
- **Device** — shared by Android + iOS; **cleared** at the start of each run.

Toggle either with its disclosure triangle. When **both** are collapsed the
panel shrinks to a fixed narrow size that just fits the button labels; when one
is open the panel is **resizable by hand** (drag any edge) and remembers the
width you set. Log text is selectable and **⌘C** copies it (⌘A selects all).

## Run on a worktree

**Long-press** (or **right-click**) any button to pick which git worktree to
run the task in. The chosen path is handed to devpanel.py via
`DEVPANEL_REPO_ROOT`; a plain click runs against the main checkout.

## Wait for phone unlock

Both device actions wait for the phone to be ready before launching:

- **Android** — `wait_for_android_unlock` polls `dumpsys window`'s keyguard
  flag. (If the device exposes no recognised flag — e.g. set to never lock in
  developer options — it doesn't block.)
- **iOS** — deploys to the device devicectl sees on a **cable** (Wi-Fi if none),
  never the first paired one listed — switching iPhones just works. Then two
  gates, both verified against a real device:
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

This compiles `build/DevPanel.app` and opens it (top-right of the screen). It has
no Dock icon (`LSUIElement`). Drag the panel by its background to reposition. The
panel stays above other windows and follows you across Spaces.

**Hide / show / quit.** Since there's no Dock icon, the app lives in the menu
bar (a ☰ slider icon):

- The **yellow** traffic-light *hides* the panel (it doesn't quit).
- **Click** the menu-bar icon to toggle the panel back on/off.
- **Right-click** (or control-click) the menu-bar icon for a *Show / Hide* +
  *Quit* menu — the way to quit while the panel is hidden.
- The panel's **`✕`** still quits outright.

The absolute path to `scripts/` (where `devpanel.py` lives) is baked into the app's Info.plist at build
time, so the `.app` works even if copied to `/Applications`. **Re-run the build
if you move the repo.**

## Notes

- **iOS** needs the device paired/trusted and your signing team set on the
  `Kinowo` target in Xcode once; the script passes `-allowProvisioningUpdates`
  but can't invent a team. The bundle id is read from the freshly built `.app`,
  so a team-namespace change to it doesn't break the launch step.
- **Android** signs with the release key from the Touch ID vault (the only
  source; there is no local keystore file): the panel runs `runOnDevice` through
  `scripts/android-signed`, which writes the keystore to a temp file for the
  length of the build. Expect a Touch ID prompt when the vault is locked.
- **Android over WiFi** works with no extra setup in the panel: on the phone
  enable Settings → Developer options → Wireless debugging, then pair this
  machine once (`adb pair <ip>:<pairing-port> <6-digit-code>`, ports from the
  Wireless debugging screen) and `adb connect <ip>:<port>` (`adb mdns services`
  lists the connect port — it differs from the pairing one). Pairing is
  remembered, so later sessions only need `adb connect`. Note that a phone still
  carrying the **Play** build refuses the local install (different signing key
  ⇒ `INSTALL_FAILED_UPDATE_INCOMPATIBLE`, which `runOnDevice` recovers from by
  uninstalling — that CLEARS the app's data; a lower local versionCode gives
  `INSTALL_FAILED_VERSION_DOWNGRADE`, fixed with
  `KINOWO_VERSION_CODE=<play+1>`).

## Tests

```bash
bash tools/devpanel/test.sh      # = python3 -m unittest tools/devpanel/test_devpanel.py
```

`test_devpanel.py` runs every action against `ScriptedHost` — the `Host` seam
with scripted adb / devicectl replies (including a real captured
`devicectl list devices`), so device picking, the unlock waits and the retry on
a locked launch are tested without a phone. `free_port` runs
against a real process. It then compiles the Swift app and runs its headless
self-test, which drives the real `CommandRunner` (output streaming, the
`DEVPANEL_REPO_ROOT` env path, and the actual `devpanel.py` command line).
