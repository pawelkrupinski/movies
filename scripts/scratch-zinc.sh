#!/usr/bin/env bash
# A stand-in Zinc analysis for the shell test scripts: what scripts/zinc_orphans.py reads of a real one.
#
# scratch_zinc_build <checkout> <module> <class path>...   e.g. scratch_zinc_build "$tree" worker tools/Kept.class
#   writes <module>/target/scala-3.10.0/classes/<class path> (and its .tasty) for each, and <module>/target/scala-3.10.0/
#   zinc/inc_compile_3.zip listing them as products -- each a ${BASE}-relative protobuf string, as sbt writes them.
#   Class files written after it are products no build produced: an orphan, as IntelliJ's JPS build leaves them.

scratch_zinc_build() {
  local checkout=$1 module=$2 c target
  shift 2
  target=$checkout/$module/target/scala-3.10.0
  for c in "$@"; do
    mkdir -p "$target/classes/$(dirname "$c")"
    echo class > "$target/classes/$c"; echo tasty > "$target/classes/${c%.class}.tasty"
  done
  mkdir -p "$target/zinc"
  python3 - "$target/zinc/inc_compile_3.zip" "$module" "$@" <<'PY'
import sys, zipfile
out, module, classes = sys.argv[1], sys.argv[2], sys.argv[3:]
def string(s):
    b = s.encode(); n = len(b); varint = bytearray()
    while True:
        varint.append((n & 0x7F) | (0x80 if n > 0x7F else 0)); n >>= 7
        if not n: return b"\x0a" + bytes(varint) + b
body = b"".join(string("${BASE}/%s/target/scala-3.10.0/classes/%s" % (module, c)) for c in classes)
body += string("${BASE}/%s/src/main/scala/Source.scala" % module)
with zipfile.ZipFile(out, "w") as z:
    z.writestr("inc_compile.bin", body)
PY
}
