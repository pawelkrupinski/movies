{ config, lib, ... }:

let
  # THE HETZNER VOLUME'S DEVICE PATH, and the one value in this repository that cannot be known
  # until Terraform has run. Hetzner exposes an attached volume as
  # /dev/disk/by-id/scsi-0HC_Volume_<volume id>, and the id is allocated at create time.
  #
  # Read it with `terraform output -json hosts | jq -r '."mongo-1".volumes."mongo-data"'` and paste
  # it here. The assertion below is what stops this being a silent trap: with the placeholder left
  # in, evaluation FAILS -- because the alternative failure is far worse. A `fileSystems` entry
  # pointing at a device that does not exist does not stop the boot (`nofail` is not even needed;
  # systemd simply times the mount unit out), and mongod then starts and writes the production
  # database to the ROOT disk, on a host whose Terraform says it is on an 80GB volume, looking
  # perfectly healthy while doing it. That is exactly the failure mode the `mount_point` comment in
  # modules/server/vars.tf warns about, and an eval-time error is the cheapest possible version of
  # discovering it.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_REPLACE_AFTER_FIRST_TERRAFORM_APPLY";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/mongodb.nix
    ../../modules/roles/wireguard-fly.nix
  ];

  assertions = [
    {
      assertion = !lib.hasInfix "REPLACE_AFTER_FIRST_TERRAFORM_APPLY" volumeDevice;
      message = ''
        mongo-1: the Hetzner volume id has not been filled in. Run

            terraform -chdir=infra/terraform output -json hosts \
              | jq -r '."mongo-1".volumes."mongo-data"'

        and substitute it into `volumeDevice` in infra/nix/hosts/mongo-1/default.nix.

        This is an eval-time failure on purpose: if the mount is wrong, mongod silently writes the
        production database to the root disk instead and nothing reports it.
      '';
    }
  ];

  networking.hostName = "mongo-1";

  fleet = {
    role = "mongo";
    privateAddress = "10.20.0.10";
  };

  # `nofail` is deliberately ABSENT. This host exists to serve one database; a boot that comes up
  # without the database's disk attached is not a degraded success, it is a machine that must stop
  # and be looked at before mongod gets the chance to start writing somewhere else.
  fileSystems."/var/lib/mongodb" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  sops.defaultSopsFile = ../../secrets/mongo-1.yaml;

  # Written by the Ubuntu install this host was converted from. Left at the release the machine was
  # first built on, per the usual rule: `stateVersion` records which release's stateful defaults the
  # data on disk was created under, and bumping it to "keep current" silently changes those defaults
  # underneath data that predates them.
  system.stateVersion = "26.05";
}
