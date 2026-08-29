#!/usr/bin/env bash
#
# The ONLY thing the CI deploy key can run on this host. Three operations, all inert.
#
# PORTED VERBATIM IN SUBSTANCE from bitcashier's nix/files/nix-stage-endpoint.sh. Nothing about it
# is fleet-specific: it is the whole of the trust boundary between a build server and a running
# machine, and the reasoning below is why each piece cannot be simplified away.
#
# It is an `authorized_keys` forced command, so what the client asked for arrives in
# $SSH_ORIGINAL_COMMAND and is ignored as an instruction -- it is read as DATA, matched against the
# cases below, and anything else is refused. There is no shell on this path.
#
# WHY A WRAPPER AND NOT `command="nix-store --serve --write"` DIRECTLY. Copying a closure and
# KEEPING it are two different operations, and a copied path that nothing refers to is garbage: the
# next `nix-collect-garbage` deletes it -- nightly on this fleet, see nix/modules/fleet/nix-gc.nix
# -- so the host would silently stop being staged while everything still reported success. A single
# forced command cannot do both, so this is the smallest thing that can.
#
# WHY IT NEEDS NO PRIVILEGE. The pin is written inside this account's own directory, and
# /nix/var/nix/gcroots/auto/<name> is a symlink TO that symlink, created declaratively by
# nix/modules/fleet/deploy-staging.nix. Nix follows indirect roots, so a closure stays alive without
# this account ever writing to /nix/var/nix/gcroots or being root. It is not in `trusted-users`
# either: the paths it sends are accepted only because CI signed them and this host lists the public
# half in `trusted-public-keys`, which is what stops this key inserting an arbitrary store path.
#
# IT CANNOT ACTIVATE ANYTHING. Nothing here runs switch-to-configuration, touches the system
# profile, or restarts a unit. A staged closure sits in the store until something activates it --
# on this fleet that is either a person or nix/modules/fleet/auto-apply.nix, and the latter makes
# its own decision locally from the closure's contents. CI gains no verb by writing the pin.
set -uo pipefail

pin_directory="${NIX_STAGE_PIN_DIR:?NIX_STAGE_PIN_DIR is not set}"
requested="${SSH_ORIGINAL_COMMAND:-}"

refuse() {
    echo "nix-stage-endpoint: refused. This key may only receive and pin a signed closure." >&2
    echo "  asked for: ${requested}" >&2
    exit 1
}

case "${requested}" in
    # MATCHED LOOSELY, EXECUTED STRICTLY. `nix copy --to ssh://...` invokes `nix-store --serve
    # --write`, but the exact string has varied between nix versions, and a forced command that
    # stopped matching would fail every deploy with a refusal that looks like a permissions problem
    # -- the single most misleading error this path can produce. So the PREFIX decides the case and
    # the command run is this file's own fixed one; nothing from the client is passed through as an
    # argument.
    "nix-store --serve"*)
        exec nix-store --serve --write
        ;;

    # READ-ONLY, so the staging tool can say "already running" instead of copying blind -- which on
    # this fleet is the difference between a 3-second no-op and pushing a gigabyte over the public
    # internet to each of three hosts. It reveals the store path of the running system, which is not
    # a secret: it is derivable from the repository by anyone who can read it.
    "current")
        readlink -f /run/current-system
        ;;
    "pin "*)
        path="${requested#pin }"
        # A store path and nothing else: no traversal, no relative path, no second word. This is the
        # ONE place a value from the client becomes a filename, so it is checked rather than trusted.
        case "${path}" in
            /nix/store/*[!\ ]) ;;
            *) refuse ;;
        esac
        [[ "${path}" == *".."* ]] && refuse
        [[ -e "${path}" ]] || { echo "nix-stage-endpoint: ${path} is not on this host" >&2; exit 1; }
        # It must be a system closure, not any old path -- otherwise this pins arbitrary storage
        # against a name that says a deploy is staged, and the auto-applier would then read a pin
        # it cannot dry-activate and report "undetermined" for ever.
        [[ -x "${path}/bin/switch-to-configuration" ]] || {
            echo "nix-stage-endpoint: ${path} is not a NixOS system closure" >&2; exit 1; }
        mkdir -p "${pin_directory}"
        # `ln -sfn` UNLINKS AND RECREATES, which is a change to the DIRECTORY ENTRY rather than to
        # the symlink -- and that is what nix/modules/fleet/auto-apply.nix's path unit watches for.
        # Writing the pin is therefore also what triggers the applier, within about a second.
        ln -sfn "${path}" "${pin_directory}/staged-system"
        echo "pinned ${path}"
        ;;
    *)
        refuse
        ;;
esac
