// `mongosh --nodb --quiet [--eval <code>] --file <a.js> --file <b.js> …` for plain Node. Every file and --eval runs in ONE
// context, in order, as mongosh loads them: their top-level declarations meet, `print` / `quit` mean what they mean
// there, and a file sees `require` and its own `__dirname` (mirror-targets.js finds its list by it).
//
//   node scripts/local-mirror/nodb.js [--eval <code>] --file <a.js> …
//
// Two users: rules-test.sh runs the specs through this when mongosh is absent (a CI runner), and mirror.sh runs every
// check that needs no server through it (the collection-list digest, the what-if fence) -- never mongosh, whose start
// costs ~5x node's and, on macOS, checks in with the window server as a GUI app (no-app-registration.js).
const fs = require("fs");
const path = require("path");
const vm = require("vm");

const context = vm.createContext({
  print: (...values) => console.log(...values),
  quit: code => process.exit(code === undefined ? 0 : code),
  require,
});
const args = process.argv.slice(2);
for (let i = 0; i < args.length; i++) {
  const flag = args[i];
  if (flag === "--nodb" || flag === "--quiet") continue;
  const value = args[++i];
  if (value === undefined) { console.error(`nodb.js: ${flag} needs a value`); process.exit(2); }
  if (flag === "--eval") vm.runInContext(value, context, { filename: "--eval" });
  else if (flag === "--file") {
    context.__dirname = path.dirname(path.resolve(value));
    vm.runInContext(fs.readFileSync(value, "utf8"), context, { filename: value });
  }
  else { console.error(`nodb.js: unknown argument ${flag}`); process.exit(2); }
}
