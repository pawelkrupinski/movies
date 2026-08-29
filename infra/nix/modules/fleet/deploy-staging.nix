# Let CI put a closure ON this host without being able to run it, or anything else.
#
# PORTED FAITHFULLY from bitcashier's `nix/modules/fleet/deploy-staging.nix`. The security argument
# is reproduced in full below rather than summarised, because every part of it is load-bearing and
# the tempting simplifications are each one of the things it refuses.
#
# WHAT THIS BUYS AND WHY IT IS SHAPED THIS WAY. The obvious design is push-from-CI: give the runner
# a key that can `sudo nixos-rebuild` on every host. That would make the least managed machine on
# the estate the most privileged one -- and here that machine is a GitHub-hosted `ubuntu-latest`
# runner nobody owns, which makes the objection stronger rather than weaker. It is ANSWERED here
# rather than overridden, and "copy only, no activation" does NOT answer it on its own:
#
#   * `nix copy` normally requires the sending user to be in `trusted-users`, and a trusted nix user
#     can insert ARBITRARY paths into the store. That is a route to root the moment anything
#     activates. So this account is deliberately NOT trusted -- see `nix.settings.trusted-users` in
#     ./default.nix, which names `root` and `@wheel` and pointedly not `nixdeploy`. What lets its
#     paths in instead is that CI SIGNS them and this host lists the public half in
#     `trusted-public-keys`, so the key can deliver exactly the closures CI built and nothing it
#     invents.
#   * The key is a FORCED COMMAND (../../files/nix-stage-endpoint.sh) with `restrict`, so it has no
#     shell, no port forwarding and no pty. It can receive a signed closure and pin it. It cannot
#     run switch-to-configuration, touch the system profile, restart a unit, or read anything.
#   * It is an unprivileged account that writes only inside its own directory. The GC root is
#     INDIRECT: /nix/var/nix/gcroots/auto/... points at a symlink under /var/lib/nixdeploy, which is
#     what the endpoint rewrites. Nix follows that, so a staged closure survives garbage collection
#     without this account ever writing to /nix/var/nix/gcroots or being root.
#
# So a compromise of the runner can leave a malicious closure sitting on a host. It cannot start it.
# That residual risk is the floor for any arrangement where CI builds artefacts at all.
#
# ONE THING IS DIFFERENT HERE AND IT MAKES THE BOUNDARY MATTER MORE, NOT LESS: bitcashier's runner
# is inside its own private network, so the ssh path is already unreachable from the internet. This
# key is used from GitHub's shared infrastructure over the public NIC. Nothing above depends on
# where the client is -- that is the point of a forced command -- but it is why none of it should
# be relaxed for convenience.
#
# DISABLED UNTIL A KEY IS SET. With `authorizedKey` empty -- the default -- no account is created
# and nothing about the host changes.
{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.fleet.deployStaging;
  pinDirectory = "/var/lib/nixdeploy";

  endpoint = pkgs.writeShellScript "nix-stage-endpoint" ''
    export NIX_STAGE_PIN_DIR=${pinDirectory}
    export PATH=${lib.makeBinPath [ config.nix.package pkgs.coreutils ]}:$PATH
    exec ${pkgs.bash}/bin/bash ${../../files/nix-stage-endpoint.sh}
  '';
in
{
  options.fleet.deployStaging = {
    authorizedKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        The PUBLIC half of the ssh key CI stages closures with, as one authorized_keys line WITHOUT
        options -- the forced command and `restrict` are added here, so that they cannot be left off
        at the call site. Empty disables the whole mechanism and creates no account.

        Set it FLEET-WIDE in a host-agnostic place, not per host: a machine minted tomorrow is
        covered the moment it imports the fleet module, with no roster to remember to add it to.
        A hand-kept roster of hosts is exactly the failure this mechanism exists to end, and it
        would be a poor thing to reintroduce in its own wiring.
      '';
    };

    trustedPublicKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "kinowo-infra-1:<base64>" ];
      description = ''
        Nix signing public keys, `<name>:<base64>`, whose signatures this host will accept on
        incoming store paths. This is what makes the staging account safe to leave OUT of
        `trusted-users`: without a matching signature the copy is refused with "cannot add path ...
        because it lacks a signature", which is the correct and legible failure.

        Generate the pair with `nix key generate-secret --key-name kinowo-infra-1`; the secret half
        becomes the `NIX_STAGE_SIGNING_KEY` repository secret and the public half goes here.
      '';
    };
  };

  config = {
    # WHICH COMMIT IS THIS HOST RUNNING. Without this, `nixos-version --configuration-revision`
    # answers "unknown" everywhere and nothing can compare a host against main.
    #
    # IT IS ANSWERABLE ONLY BECAUSE CI BUILDS FROM A REAL CHECKOUT. A flake sees only tracked files,
    # so a build from an rsynced tree with no `.git` has no `self.rev` to read -- which is why
    # .github/workflows/nix-stage-closures.yaml checks out with `fetch-depth: 0`. `dirtyRev` covers
    # a build from a modified tree, and the fallback says outright that the build had no revision
    # rather than implying one.
    #
    # `inputs` COMES FROM THE FLAKE'S `specialArgs`. If this module is ever evaluated without it,
    # that is a build error naming this line, which is the right place to find out.
    system.configurationRevision =
      inputs.self.rev or inputs.self.dirtyRev or "no-revision-tree-was-not-a-git-checkout";

    nix.settings.trusted-public-keys =
      lib.mkIf (cfg.trustedPublicKeys != [ ]) cfg.trustedPublicKeys;

    users.groups.nixdeploy = lib.mkIf (cfg.authorizedKey != "") { };

    users.users.nixdeploy = lib.mkIf (cfg.authorizedKey != "") {
      isSystemUser = true;
      group = "nixdeploy";
      home = pinDirectory;
      createHome = true;
      # A shell is required for a forced command to run at all; it is never reached interactively,
      # because `restrict` denies a pty and the forced command replaces whatever was asked for.
      shell = pkgs.bashInteractive;
      openssh.authorizedKeys.keys = [
        ''command="${endpoint}",restrict ${cfg.authorizedKey}''
      ];
    };

    systemd.tmpfiles.rules = lib.mkIf (cfg.authorizedKey != "") [
      # 0700, NOT 0755, and 0700 is what the machine does anyway: `createHome` makes the home 0700
      # during activation and wins over a looser rule here, so declaring 0755 would be both
      # unachieved and the weaker of the two. Nothing needs to read this directory except root --
      # which ignores the mode, and which is what follows the gc root below.
      "d ${pinDirectory} 0700 nixdeploy nixdeploy -"
      # THE INDIRECT GC ROOT, and it is not optional. gcroots/auto entries are FOLLOWED to whatever
      # they point at, so this keeps the staged closure alive while the only thing the deploy
      # account rewrites is its own symlink inside its own directory. Without it a copied closure is
      # unreferenced and ./nix-gc.nix's next nightly collection deletes it -- the host would stop
      # being staged with nothing reporting that it had, and the next merge would silently re-copy
      # the same gigabyte.
      "L+ /nix/var/nix/gcroots/auto/nixdeploy-staged - - - - ${pinDirectory}/staged-system"
    ];
  };
}
