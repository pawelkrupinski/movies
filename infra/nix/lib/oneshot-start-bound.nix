# `oneshotStartBound serviceConfig`: how long systemd lets a oneshot's start job run, in seconds, or
# null when it is unbounded or this cannot read it -- the direction ../modules/fleet/auto-apply.nix's
# assertion refuses.
#
# UNBOUNDED UNLESS DECLARED. systemd applies DefaultTimeoutStartSec to every service type but
# oneshot, whose timeouts "are disabled by default" (systemd.service(5)): a oneshot with neither
# TimeoutStartSec nor TimeoutSec may hold its start job -- and a switch waiting on it -- for ever.
# `infinity` and `0` say the same out loud. A span this does not read (`1h 30min`) is null too.
#
# Builtins only, so infra/test/test_oneshot_start_bound.sh evaluates it without nixpkgs.
serviceConfig:
let
  declared =
    if serviceConfig ? TimeoutStartSec then serviceConfig.TimeoutStartSec
    else serviceConfig.TimeoutSec or null;
  # a merged option: the last definition is the one systemd reads
  value = if builtins.isList declared then builtins.elemAt declared (builtins.length declared - 1) else declared;
  parsed = builtins.match "([1-9][0-9]*) *(s|sec|m|min|h|hr)?" (toString value);
  factor = { s = 1; sec = 1; m = 60; min = 60; h = 3600; hr = 3600; };
  unit = builtins.elemAt parsed 1;
in
if declared == null || parsed == null then null
else builtins.fromJSON (builtins.elemAt parsed 0) * (if unit == null then 1 else factor.${unit})
