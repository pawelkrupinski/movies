{ ... }:

let
  # THE HETZNER VOLUME'S DEVICE PATH. Allocated by Terraform at create time and read back with
  # `terraform -chdir=infra/terraform output -json hosts | jq -r '."mongo-1".volumes."mongo-data"'`;
  # Hetzner exposes an attached volume as /dev/disk/by-id/scsi-0HC_Volume_<volume id>.
  #
  # WHY A WRONG VALUE HERE IS WORSE THAN A BUILD FAILURE, and why it is worth checking against that
  # command rather than trusting this line: a `fileSystems` entry naming a device that does not
  # exist DOES NOT STOP THE BOOT. systemd times the mount unit out and carries on, mongod starts,
  # and the production database is written to the 40GB ROOT disk -- on a host whose Terraform says
  # it is on an 80GB volume, looking perfectly healthy the whole time. That is precisely the trap
  # the `mount_point` comment in terraform/modules/server/vars.tf describes.
  #
  # The guard against it is the missing `nofail` below, not this comment: without it the mount is a
  # hard boot dependency, so a wrong id fails loudly at boot instead of quietly at runtime.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_106735228";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/mongodb.nix
    ../../modules/roles/wireguard-fly.nix
  ];

  networking.hostName = "mongo-1";

  fleet = {
    role = "mongo";
    privateAddress = "10.20.0.10";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `mongo_1_ipv4` there.
    publicAddress = "2.28.56.140";
  };

  # `nofail` is deliberately ABSENT. This host exists to serve one database; a boot that comes up
  # without the database's disk attached is not a degraded success, it is a machine that must stop
  # and be looked at before mongod gets the chance to start writing somewhere else.
  fileSystems."/var/lib/mongodb" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  # THE DATABASE ITSELF.
  #
  # `replSetName` IS "rs0" BECAUSE THAT IS WHAT THE FLY DATABASE CALLS ITSELF -- checked against the
  # running instance on 2026-08-29 (`replSetGetStatus` reports set "rs0", 1 member, mongod 7.0.39),
  # not chosen. The migration restores a dump into this instance, and the app opens CHANGE STREAMS
  # against it; a standalone mongod has no oplog and change streams simply do not exist there, so
  # the web tier would hydrate once at boot and then never see another update -- serving stale
  # showtimes indefinitely while looking completely healthy. Single-node is still a real replica
  # set for this purpose.
  #
  # WiredTiger IS PINNED AT 1GB ON A 4GB BOX. mongod's default is (RAM - 1GB) / 2, which leaves the
  # cache free to grow into memory the rest of the system needs; this fleet's Fly-hosted predecessor
  # was OOM-killed that way. 1GB against a 70MB dataset is enormous headroom regardless.
  fleet.mongodb = {
    enable = true;
    replSetName = "rs0";
    wiredTigerCacheSizeGB = 1.0;

    # 127.0.0.1 for the local mongodump timer, the private address for anything on this fleet, and
    # the Fly 6PN address for the actual application traffic. NEVER the public IP -- see
    # roles/mongodb.nix, which asserts the loopback entry is present because the backup timer
    # depends on it.
    # The third entry is the WireGuard address and it is the one the APPLICATION uses; the other
    # two serve the local backup timer and this fleet's own Prometheus. Still never the public IP.
    bindAddresses = [ "127.0.0.1" "10.20.0.10" "fdaa:74:b6b5:a7b:35c:d566:7af5:7502" ];
  };

  # Opens 27017 on the PRIVATE interface only. The Fly apps do not arrive that way -- they come down
  # the WireGuard tunnel below -- so this is for Prometheus's mongod scrape and for an operator on
  # monitoring-1, not for the application.
  fleet.firewall.mongo = true;

  # THE TUNNEL THE APPLICATION ACTUALLY ARRIVES ON, and the reason mongod is never exposed
  # publicly. mongo-1 joins Fly's 6PN as a peer, so `kinowo`, `kinowo-worker` and the rest keep
  # reaching their database over Fly's private network exactly as they do today -- only the HOST in
  # MONGODB_URI changes.
  #
  # THESE VALUES CAME FROM `fly wireguard create personal arn kinowo-mongo-1`, 2026-08-29, and the
  # peer is named there so it can be found and revoked. `arn` because that is where the web and
  # worker apps run; the gateway region decides which Fly edge the tunnel terminates at, so any
  # other choice adds a transatlantic hop to every query for no reason.
  #
  # NOTE THE ENDPOINT: this side DIALS OUT to arn1.gateway.6pn.dev. Fly does not dial in, which is
  # why roles/wireguard-fly.nix sets no `listenPort` and why neither firewall opens 51820 -- return
  # traffic is an established flow. If that ever changes, all three move together.
  #
  # THE PRIVATE KEY CANNOT BE RECOVERED. flyctl prints it once, at creation; losing it means
  # removing the peer and adding a new one. It is in nix/secrets/mongo-1.yaml and nowhere else.
  fleet.wireguardFly = {
    enable = true;
    address = "fdaa:74:b6b5:a7b:35c:d566:7af5:7502/120";
    peerPublicKey = "tyYPi0DmwNDs3YEhnm4CeNy5I9m2QSsdry4H46Zfr3M=";
    peerEndpoint = "arn1.gateway.6pn.dev:51820";
    allowedIPs = [ "fdaa:74:b6b5::/48" ];
  };

  sops.defaultSopsFile = ../../secrets/mongo-1.yaml;

  # Written by the Ubuntu install this host was converted from. Left at the release the machine was
  # first built on, per the usual rule: `stateVersion` records which release's stateful defaults the
  # data on disk was created under, and bumping it to "keep current" silently changes those defaults
  # underneath data that predates them.
  system.stateVersion = "26.05";
}
