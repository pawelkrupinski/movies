terraform {
  required_providers {
    hcloud = {
      source  = "hetznercloud/hcloud"
      version = "~> 1.0"
    }
  }

  # Terraform Cloud, execution mode Local -- the same backend bitcashier/infra uses, in the same
  # organisation, in its own workspace. Local execution means TFC stores and locks the state and
  # nothing else: the plan runs on whoever's machine typed `terraform plan`, reading
  # HCLOUD_TOKEN from the environment, so no Hetzner credential is ever uploaded.
  #
  # `kinowo-infra`, NOT bitcashier's `infra-repo`. Two estates in one workspace share a lock and a
  # state file, so a plan for either would refresh every resource of the other and one careless
  # `-destroy` would reach across.
  backend "remote" {
    hostname     = "app.terraform.io"
    organization = "bitcashier"

    workspaces {
      name = "kinowo-infra"
    }
  }
}
