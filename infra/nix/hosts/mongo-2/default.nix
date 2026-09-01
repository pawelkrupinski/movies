{ ... }:

let
  # THE HETZNER VOLUME'S DEVICE PATH, read back with
  # `terraform -chdir=infra/terraform output -json hosts | jq -r '."mongo-2".volumes."mongo-data-fsn1"'`.
  # See the long note in ../mongo-1/default.nix for why a wrong id here is worse than a build
  # failure, and why the absent `nofail` below is the guard rather than the comment.
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

    # NO wireguard-fly.nix, UNLIKE mongo-1, AND THE ABSENCE IS THE DECISION. That peer existed so
    # the Fly-hosted web and worker tiers could reach mongod over 6PN; both moved to k3s on
    # 2026-08-29 and every client has reached the private address directly ever since, so mongo-1's
    # own configuration already calls the tunnel "a tunnel the application no longer arrives on".
    # Recreating it here would mean a second `fly wireguard create`, a second private key that
    # flyctl prints exactly once, and a second thing to revoke -- to carry no traffic. If something
    # ever needs it again, add the import and the peer together.
    ../../modules/fleet/logs.nix
  ];

  networking.hostName = "mongo-2";

  fleet = {
    role = "mongo";
    privateAddress = "10.20.0.13";

    # .13 AND NOT .10, WHICH IS THE PRICE OF NOT TAKING THE DATABASE DOWN. Two machines cannot hold
    # one private address, and the only migration that never drops a query needs both of them up at
    # once -- the new member syncing from the old while the old still serves. So the address moves
    # by REPLICA-SET MEMBERSHIP rather than by being reassigned, and the clients follow the set
    # instead of following an address (see the `repoint` phase of migrate-mongo-replica.yml, which
    # takes `directConnection=true` out of MONGODB_URI for exactly this reason).
    publicAddress = "178.105.221.61";
  };

  # `nofail` deliberately ABSENT, as on mongo-1: a mongod that comes up without its data disk is not
  # a degraded success, it is a machine that must stop before it starts writing somewhere else.
  fileSystems."/var/lib/mongodb" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  fleet.mongodb = {
    enable = true;
    replSetName = "rs0";

    # THE ONE LINE THAT MAKES THIS HOST SAFE TO BUILD BESIDE A LIVE DATABASE.
    #
    # `mongodb-init-replicaset` reacts to `NotYetInitialized` by creating the set. An empty dbPath
    # answers `NotYetInitialized`, so without this the machine would come up having given itself its
    # OWN one-member `rs0` -- same name, own history, no relationship to the real one -- and it
    # would look completely healthy doing it. The real primary's `rs.add()` would then fail talking
    # about configuration versions rather than about the actual problem.
    #
    # This host is an ADDITION to a set that already exists. The current primary does the adding.
    initiateReplicaSet = false;

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
    # every client. NO 6PN ENTRY, because this host has no Fly peer (see the import list); mongo-1
    # carries one only for a tunnel nothing uses. Never the public IP.
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

  sops.defaultSopsFile = ../../secrets/mongo-2.yaml;

  # The release this machine was built on. See the note in ../mongo-1/default.nix: this records
  # which release's stateful defaults the data on disk was created under, and is not a "keep
  # current" knob.
  system.stateVersion = "26.05";
}
