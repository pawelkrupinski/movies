# Who can log in to these machines, and as what.
#
# PORTED FROM bitcashier's `nix/modules/fleet/accounts.nix` WITH ITS SOURCE OF TRUTH INVERTED, and
# that inversion is the only interesting thing about this file. There, the key list is GENERATED
# from Puppet hieradata by a sync script and a checker fails when the two disagree, because most of
# that estate is still Puppet-managed and a key revoked in one place but not the other is a
# revocation that did not happen. Here there is no second manager and no second list: this file IS
# the authority, and the keys are written out rather than imported so that `git log` on one file is
# the complete history of who has had access to this fleet.
#
# `users.mutableUsers = false` is what makes that claim true rather than aspirational. With it,
# /etc/passwd and every authorized_keys file are rebuilt from this declaration on each activation,
# so an account or a key added by hand on the machine is REMOVED by the next deploy instead of
# quietly surviving it. Without it this file would describe a floor, not the state.
{ ... }:

let
  # The operator. One human, named rather than shared: `root` is reachable too (see below) but
  # every ordinary login is a person escalating as themselves, so `sudo` in the journal names
  # somebody.
  #
  # Read from /Users/pawel/.ssh/id_ed25519.pub and embedded verbatim. If this key is ever rotated,
  # the ONLY safe order is: add the new key here, deploy, verify a login with it, and only then
  # delete the old line -- because the deploy that removes a key is applied by that same key.
  operatorKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJew5N81VkQghbNbGSpGXk5LPsZG3TkWRwFtPo5lrVVg pawel.krupinski@gmail.com"
  ];
in
{
  users.mutableUsers = false;

  users.users.pawel = {
    isNormalUser = true;
    description = "Pawel Krupinski";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = operatorKeys;
  };

  # ROOT BY KEY, AND IT IS NOT OPTIONAL HERE -- it is how this fleet is built and deployed.
  #
  # nixos-anywhere installs onto a rescue system as root; colmena's default `targetUser` is root
  # and it activates by running `switch-to-configuration` directly; and a machine whose `pawel`
  # account failed to render -- which is a thing that has happened on the fleet this was ported
  # from, where a mis-evaluated `users.users` created two accounts that were meant to be absent --
  # needs a recovery path that does not depend on the part that broke.
  #
  # `hashedPassword = "!"` is the half that keeps this narrow: `!` is not a hash any password can
  # produce, so root has NO password at all, and sshd's `PermitRootLogin = "prohibit-password"`
  # (./default.nix) means the only way in is a key listed right here. There is no console
  # login either -- Hetzner's console reaches a login prompt that no password satisfies.
  users.users.root = {
    hashedPassword = "!";
    openssh.authorizedKeys.keys = operatorKeys;
  };

  # PASSWORDLESS SUDO FOR `wheel`. Every administrative path onto this fleet is an ssh key, so a
  # password prompt here would be a prompt for a password nobody has set -- and `users.mutableUsers
  # = false` means nobody CAN set one. The choice is between passwordless sudo and an operator who
  # cannot escalate; the second is not a security posture, it is an outage waiting for the first
  # incident.
  #
  # `lib.mkDefault` on neither, deliberately: a host that wanted a password prompt would be a host
  # that could not be recovered, and making that easy to write by accident is not a kindness.
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # `nixdeploy`, the CI staging account, is NOT here and must never be: it is created by
  # ./deploy-staging.nix as a system user whose key carries a forced command, and adding it
  # to `wheel` -- or to this file at all -- would hand the build server the root it deliberately
  # does not have. Named here only so that a reader auditing "who can log in" from this file alone
  # knows there is a fourth answer and where to find it.
}
