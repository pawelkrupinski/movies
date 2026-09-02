# Consumed by infra/ansible/inventory (which resolves hosts from the hcloud API by label, not from
# these) and by anybody wiring a `fileSystems` entry against a volume id. Kept because reading a
# volume id out of the Hetzner console is exactly the manual step that puts a wrong id into a nix
# file.
output "hosts" {
  value = {
    mongo-1 = {
      id         = module.mongo_1.id
      public_ip  = module.mongo_1.public_ip
      private_ip = module.mongo_1.private_ip
      volumes    = module.mongo_1.volume_ids
    }
    monitoring-1 = {
      id         = module.monitoring_1.id
      public_ip  = module.monitoring_1.public_ip
      private_ip = module.monitoring_1.private_ip
      volumes    = module.monitoring_1.volume_ids
    }
    k3s-worker-1 = {
      id         = module.k3s_worker_1.id
      public_ip  = module.k3s_worker_1.public_ip
      private_ip = module.k3s_worker_1.private_ip
      volumes    = module.k3s_worker_1.volume_ids
    }
  }
}
