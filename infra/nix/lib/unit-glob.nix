# `matchesUnit unit pattern`: does the fnmatch GLOB `pattern` name `unit`?
#
# `fleet.autoApply.{restartable,routine,neverDisturb}Units` are globs -- the applier matches them with
# Python's fnmatch, so `*` and `?` are the two wildcards and a host may well write `grafana.*` or `*`. A
# plain `elem` would read those as literals and report a host that HAS forgiven a unit as one that has
# not. This is the same translation, kept to the two wildcards fnmatch and Nix's regexes agree on.
{ lib }:
unit: pattern:
let
  escaped = lib.escapeRegex pattern;
  wildcarded = lib.replaceStrings [ "\\*" "\\?" ] [ ".*" "." ] escaped;
in
builtins.match wildcarded unit != null
