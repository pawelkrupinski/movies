{ ... }:

let
  # THE HETZNER VOLUME'S DEVICE PATH, read back with
  # `terraform -chdir=infra/terraform output -json hosts | jq -r '."mongo-1".volumes."mongo-data"'`.
  # A WRONG ID HERE IS WORSE THAN A BUILD FAILURE: a `fileSystems` entry naming a device that does
  # not exist does NOT stop the boot on its own -- systemd times the mount out, mongod starts, and
  # the production database is written to the ROOT disk while everything looks healthy. The guard
  # against that is the missing `nofail` below, not this comment.
  #
  # A DIFFERENT VOLUME FROM mongo-1'S, NECESSARILY. A Hetzner volume cannot leave its location, so
  # the fsn1 machine could not be given the nbg1 disk; it is handed an empty one and fills it from
  # the replica set. That is the whole reason this host joins by REPLICATION rather than by being
  # handed a copy of a disk -- see ../../../ansible/playbooks/migrate-mongo-replica.yml.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_106767580";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/mongo-ci-read.nix
    ../../modules/roles/mongodb.nix
    ../../modules/roles/mongodb-exporter.nix

    # NO FLY 6PN PEER, AND THE ABSENCE IS THE DECISION. Such a peer existed on the monitoring host
    # so the Fly-hosted web and worker tiers could reach mongod over 6PN; both moved to k3s on
    # 2026-08-29 and every client has reached the private address directly ever since. The role that
    # declared it was deleted on 2026-09-04 with the rest of the Fly integration.
    ../../modules/fleet/logs.nix
  ];

  networking.hostName = "mongo-1";

  fleet = {
    role = "mongo";
    privateAddress = "10.20.0.13";

    # .13 AND NOT .10, EVEN THOUGH THIS HOST NOW CARRIES THE OLD ONE'S NAME. Two machines cannot
    # hold one private address, and a migration that never drops a query needs both up at once --
    # the new member syncing from the old while the old still serves -- so the replacement had to be
    # given a free address. Once the corpus was here the NAME could be reclaimed; the address could
    # not, because reclaiming it would mean another migration to undo a difference nothing reads.
    #
    # NOTHING READS IT: clients follow the SET, not an address, since the `repoint` phase of
    # migrate-mongo-replica.yml took `directConnection=true` out of MONGODB_URI. That is also what
    # makes the next move of this database cheap.
    publicAddress = "178.105.221.61";
  };

  # `nofail` deliberately ABSENT: a mongod that comes up without its data disk is not a degraded
  # success, it is a machine that must stop before it starts writing somewhere else.
  fileSystems."/var/lib/mongodb" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  fleet.mongodb = {
    enable = true;
    replSetName = "rs0";

    # TRUE AGAIN, AND ONLY BECAUSE THIS IS ONCE MORE THE ONLY MONGO HOST.
    #
    # It was FALSE while this machine existed as `mongo-2` alongside the nbg1 original: an empty
    # dbPath answers `NotYetInitialized`, so a host built to JOIN an existing set will instead give
    # itself its own one-member `rs0` -- same name, own history, unrelated to the real one -- and
    # look completely healthy doing it. That is what let the corpus be replicated here without the
    # two machines forking into two databases.
    #
    # With the original retired there is nothing left to join, and a fleet rebuilt from scratch
    # would come up with mongod running and NO replica set at all -- no oplog, so no change streams,
    # so a web tier frozen at its boot. `infra/bin/check`'s `replica-set origin` step refuses a
    # configuration where no host would initiate, which is what turned this line back on rather than
    # leaving it to be noticed during a rebuild.
    initiateReplicaSet = true;

    # Must equal what the primary passes to `rs.add()`. mongod matches the member host in the set's
    # configuration against the addresses it has bound, and a member that cannot find itself refuses
    # to replicate with "No host described in new configuration ... maps to this node".
    replSetMemberHost = "10.20.0.13:27017";

    # Both carried over from mongo-1 rather than re-derived: this is the same cx23 (2 vCPU, 4GB)
    # holding the same corpus, so the reasoning in that file -- a 1GB WiredTiger cache to stop it
    # growing into memory the rest of the system needs, and a 4GB oplog for a ~12-hour change-stream
    # resume window -- applies unchanged. A member with a SHORTER oplog than its primary is its own
    # trap: it would sync fine and then narrow the resume window for every consumer the moment it
    # was promoted.
    oplogSizeMB = 4096;
    wiredTigerCacheSizeGB = 1.0;

    # Loopback for the local mongodump timer and this fleet's Prometheus, the private address for
    # every client. NO 6PN ENTRY, because there is no Fly peer anywhere on this fleet any more (see
    # the import list). Never the public IP.
    bindAddresses = [ "127.0.0.1" "10.20.0.13" ];
  };

  # Same two out-of-repo prerequisites as mongo-1 -- the `kinowo_monitor` user and its password --
  # except that here BOTH ARRIVE ON THEIR OWN: the user is a document in `admin` and replicates with
  # the initial sync, and the password is the same secret value, sealed for this host's key in
  # nix/secrets/mongo-2.yaml. Nothing has to be created by hand a second time.
  fleet.mongodbExporter.enable = true;

  fleet.mongoCiRead = {
    enable = true;
    authorizedKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINZAmY3cDz1SCSvIWh4dIYFxNXNjofJAiDYlE2UMxKoa mongo-ci-read@kinowo-ci";
  };

  fleet.firewall.mongo = true;

  fleet.logs = {
    enable = true;
    serverAddress = "10.20.0.11";
  };

  sops.defaultSopsFile = ../../secrets/mongo-1.yaml;

  # The release this machine was built on. See the note in ../mongo-1/default.nix: this records
  # which release's stateful defaults the data on disk was created under, and is not a "keep
  # current" knob.
  system.stateVersion = "26.05";
}
