// The browser's wall clock, read in one place. Every other shared script asks `wallClockNow()` instead of touching
// `Date` itself (the design rules table names this file as the one adapter for the clock), so the time a script reads
// is a single seam: page tests pin it and `pageToday()` (dates.js) layers the server's render instant over it.

/* exported wallClockNow */

function wallClockNow() {
  return Date.now();
}
