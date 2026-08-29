# Reads HCLOUD_TOKEN from the environment. It is HETZNER_API_TOKEN in the repo-root .env.local;
# infra/bin/tf exports it under the name the provider wants, so the token is never written into a
# tfvars file and never leaves the machine running the plan.
provider "hcloud" {}
