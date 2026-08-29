# The database host. Takes over from the self-hosted `kinowo-mongo` Fly app in arn.
#
# WHY MOVING IT OFF FLY IS SAFE DESPITE THE ~27ms IT ADDS between the web tier (still Fly, arn) and
# the database (here, nbg1): the web tier does ZERO Mongo round trips on a page render. It hydrates
# the whole read model into two ConcurrentHashMaps at boot and keeps them fresh over Mongo change
# streams (common/src/main/scala/services/readmodel/WebReadModel.scala), then serves responses out
# of a versioned gzip cache. The measured exposure is the boot hydrate (~13 paged round trips at 500
# docs a page, +0.35s) and change-stream delta propagation (+27ms per event, unobservable). No Mongo
# timeout in the app is tighter than 30s.
#
# The one place that DOES pay is the worker's ReadModelProjector, which writes one `replaceOne` per
# changed document sequentially under a lock -- normal churn is a handful of docs, but a full
# ~6.5k-document reconcile costs about three extra minutes. That is a batch job with no user waiting
# on it, and the fix if it ever matters is a bulkWrite, not a datacentre.
module "mongo_1" {
  source = "./modules/server"

  name        = "mongo-1"
  server_type = "cx23"
  image       = "ubuntu-26.04" # bootstrap only; the machine runs NixOS. See modules/server/vars.tf.
  location    = "nbg1"
  network_ids = [hcloud_network.kinowo.id]
  private_ip  = "10.20.0.10"

  primary_ipv4_id = hcloud_primary_ip.fleet["mongo_1_ipv4"].id
  primary_ipv6_id = hcloud_primary_ip.fleet["mongo_1_ipv6"].id

  labels = {
    role = "mongo"
    env  = "prod"
  }

  # A SEPARATE VOLUME FOR THE DATABASE, not the 40GB root disk, for two reasons that both bite
  # later rather than now. It can be grown without touching the machine (Hetzner resizes a volume
  # in place; a server's root disk can only grow by resizing the whole server type, and `keep_disk`
  # means it never shrinks back). And it survives the host: a `convert-host` re-run, a server-type
  # change, or a rebuild onto a fresh image all repartition the root disk, and the database is not
  # on it.
  #
  # 80GB against a Fly volume that has been sized for this workload for a year. It is deliberately
  # not tight -- WiredTiger wants headroom for compaction and the checkpoint it writes before it can
  # free anything, and this fleet has already learned what a full Mongo disk does (it crashes
  # mongod, and the crash is not the expensive part; the resize under pressure is).
  #
  # `mount_point` set, so Hetzner's automount is OFF and the mount is declared in
  # infra/nix/hosts/mongo-1/default.nix against /dev/disk/by-id/scsi-0HC_Volume_<id>. SETTING IT
  # HERE MOUNTS NOTHING -- if that `fileSystems` entry is missing, mongod silently writes to the
  # root disk instead and looks perfectly healthy doing it.
  volumes = {
    mongo-data = {
      size        = 80
      location    = "nbg1"
      mount_point = "/var/lib/mongodb"

      # Independently protected, so that clearing the SERVER's delete_protection to re-run a
      # conversion cannot take the database's disk with it.
      delete_protection = true
    }
  }
}
