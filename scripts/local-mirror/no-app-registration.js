// Preloaded into every mongosh the local mirror starts (`NODE_OPTIONS=--require …`, mongosh-at.sh and freshness): keeps
// mongosh from checking in with macOS's window server as a new GUI app.
//
// mongosh sets `process.title` at startup (cli-repl's run.js, unconditionally; no flag or variable turns it off), and on
// macOS libuv's title setter goes through LaunchServices, which registers the process as an application -- `lsappinfo
// list` shows a "mongosh " entry per launch. The mirror started one every ~18 s (2026-11-21), and each check-in
// disturbed the desktop's focus. Plain node, which never sets a title, does not register. So the title becomes an
// ordinary property: mongosh still reads back what it wrote, and nothing reaches LaunchServices.
let title = process.title;
Object.defineProperty(process, "title", {
  configurable: true,
  enumerable: true,
  get: () => title,
  set: value => { title = String(value); },
});
