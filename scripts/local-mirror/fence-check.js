// Prints the name of a what-if fence in force on SRC_DB (fence-rule.js), or nothing: mirror.sh's fence_holder, which
// holds its re-seed audit while one is.
//   node nodb.js --eval "var FENCE_DIR='<dir>'; var SRC_DB='kinowo_de'" --file fence-rule.js --file fence-check.js
const holding = fenceHolding(fencesIn(FENCE_DIR, SRC_DB), Date.now(), null);
if (holding !== null) print(holding);
