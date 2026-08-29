output "id" {
  value = hcloud_server.default.id
}

output "name" {
  value = hcloud_server.default.name
}

output "public_ip" {
  value = hcloud_server.default.ipv4_address
}

output "private_ip" {
  value = one(hcloud_server.default.network[*].ip)
}

# Keyed by the same key as `var.volumes`, so a host's nix config can be pointed at
# /dev/disk/by-id/scsi-0HC_Volume_<id> without anybody reading the id out of the console.
output "volume_ids" {
  value = { for k, v in hcloud_volume.default : k => v.id }
}
