#!/usr/bin/env python3
"""scripts/ci/mirror-pinned-images.sh, black-box, against a stub `skopeo` over a directory of registries' manifests.
Ported from mirror-pinned-images-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import glob
import hashlib
import os
import re
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/mirror_pinned_images.py", "scripts/ci/mirror-pinned-images.sh")
PINNED = '{"pinned":"index"}\n'
OTHER = '{"other":"index"}\n'
DIGEST = "sha256:" + hashlib.sha256(PINNED.encode()).hexdigest()
OTHER_DIGEST = "sha256:" + hashlib.sha256(OTHER.encode()).hexdigest()
GHCR_TAG = "ghcr.io/owner/mongo:8.3.17"

# The registries: $STUB_REG/<ref with / and : and @ as _> holds the manifest a ref serves. A copy writes the source's
# manifest to the destination, unless STUB_COPY_FAIL names a source host to refuse. A `login` records stdin.
SKOPEO = r"""#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
key() { local r=${1#docker://}; echo "$STUB_REG/${r//[\/:@]/_}"; }
case "$1" in
  inspect) [ -f "$(key "${*: -1}")" ] || exit 1; cat "$(key "${*: -1}")" ;;
  copy) src="${*: -2:1}"; dst="${*: -1}"
        [[ -n "${STUB_COPY_FAIL:-}" && "$src" == *"$STUB_COPY_FAIL"* ]] && exit 1
        [ -f "$(key "$src")" ] || exit 1
        cp "$(key "$src")" "$(key "$dst")" ;;
  login) cat >> "$STUB_LOGIN" ;;
esac
"""


def key(ref: str) -> str:
    return re.sub(r"[/:@]", "_", ref)


class MirrorPinnedImagesTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        os.chmod(self.write(".fakes/bin/skopeo", SKOPEO), 0o755)
        self.pins = self.write("pins.tsv", f"# a comment\nmongo\tdocker.io/library/mongo:8.3.17@{DIGEST}\t"
                                           "ghcr.io/owner/mongo\n")
        self.reg = self.path("reg")
        os.makedirs(self.reg)
        self.put(f"docker.io/library/mongo@{DIGEST}", PINNED)
        self.put("docker.io/library/mongo:8.3.17", PINNED)
        self.put(f"mirror.gcr.io/library/mongo@{DIGEST}", PINNED)
        self.out = ""

    def put(self, ref: str, manifest: str) -> None:
        self.write(os.path.join("reg", key(ref)), manifest)

    def drop(self, host: str) -> None:
        for path in glob.glob(os.path.join(self.reg, key(host) + "_*")):
            os.remove(path)

    def mirror(self, **env: str) -> int:
        """The tool over the stub registries (every one starting as upstream serves the pin); its exit status, its
        output in self.out."""
        self.write("log", "")
        self.write("login", "")
        done = self.run_tool(env={"PINNED_IMAGES": self.pins, "STUB_LOG": self.path("log"),
                                  "STUB_LOGIN": self.path("login"), "STUB_REG": self.reg, **env})
        self.out = done.output
        return done.code

    def ghcr_now(self) -> Optional[str]:
        path = os.path.join(self.reg, key(GHCR_TAG))
        if not os.path.exists(path):
            return None
        with open(path, "rb") as f:
            return "sha256:" + hashlib.sha256(f.read()).hexdigest()

    def said(self, pattern: str) -> int:
        return sum(1 for line in self.out.splitlines() if re.match(pattern, line))

    def logged(self, pattern: str) -> int:
        return sum(1 for line in self.read("log").splitlines() if re.match(pattern, line))

    def test_an_image_not_on_ghcr_yet_is_copied_there_by_digest_from_docker_hub(self) -> None:
        self.assertEqual(0, self.mirror(), self.out)
        self.assertEqual(1, self.logged(re.escape(f"copy --all --preserve-digests --retry-times 3 "
                                                  f"docker://docker.io/library/mongo@{DIGEST} docker://{GHCR_TAG}") + "$"))
        self.assertEqual(DIGEST, self.ghcr_now())
        self.assertEqual(1, self.said(re.escape(f"mongo: {GHCR_TAG} is now the pin, {DIGEST}")), self.out)

    def test_an_image_ghcr_already_serves_at_the_pin_is_left_alone(self) -> None:
        self.put(GHCR_TAG, PINNED)
        self.assertEqual(0, self.mirror(), self.out)
        self.assertEqual(0, self.logged("copy"))
        self.assertEqual(1, self.said(re.escape(f"mongo: {GHCR_TAG} is the pin, {DIGEST}")), self.out)

    def test_drift_is_flagged_naming_both_digests_and_the_pin_copied_over_it(self) -> None:
        self.put(GHCR_TAG, OTHER)
        self.assertEqual(0, self.mirror(), self.out)
        self.assertEqual(1, self.said(re.escape(f"::warning::mongo: DRIFT -- GHCR's {GHCR_TAG} is {OTHER_DIGEST}, "
                                                f"the pin is {DIGEST}")), self.out)
        self.assertEqual(DIGEST, self.ghcr_now())

    def test_docker_hub_refusing_the_copy_copies_from_mirror_gcr_io_instead(self) -> None:
        self.assertEqual(0, self.mirror(STUB_COPY_FAIL="docker.io"), self.out)
        self.assertEqual(DIGEST, self.ghcr_now())
        self.assertEqual(1, self.said(r"::warning::mongo: the copy from docker://docker.io/library/mongo@.* failed"))

    def test_no_source_to_copy_from_fails_loudly_saying_cis_pulls_skip_ghcr(self) -> None:
        self.drop("docker.io")
        self.drop("mirror.gcr.io")
        self.assertEqual(1, self.mirror(), self.out)
        self.assertEqual(1, self.said(r"::error::mongo: GHCR.s ghcr.io/owner/mongo:8.3.17 is missing after the copy, "
                                      r"not the pin .* CI pulls of mongo skip GHCR"), self.out)

    def test_a_drifted_ghcr_the_copy_cannot_fix_fails_naming_what_it_serves(self) -> None:
        self.put(GHCR_TAG, OTHER)
        self.drop("mirror.gcr.io")
        self.assertEqual(1, self.mirror(STUB_COPY_FAIL="docker"), self.out)
        self.assertEqual(1, self.said(re.escape(f"::error::mongo: GHCR's {GHCR_TAG} is {OTHER_DIGEST} after the copy")))

    def test_the_upstream_tag_moving_off_the_pin_is_only_a_notice(self) -> None:
        self.put(GHCR_TAG, PINNED)
        self.put("docker.io/library/mongo:8.3.17", OTHER)
        self.assertEqual(0, self.mirror(), self.out)
        self.assertEqual(1, self.said(re.escape(f"::notice::mongo: docker.io/library/mongo:8.3.17 now points at "
                                                f"{OTHER_DIGEST}, not the pinned {DIGEST}")), self.out)

    def test_ghcr_logged_in_as_the_actor_with_the_jobs_token_on_stdin_only(self) -> None:
        self.assertEqual(0, self.mirror(GHCR_TOKEN="tok", GITHUB_ACTOR="me"), self.out)
        self.assertEqual("tok", self.read("login"))
        self.assertEqual(0, self.logged(".*tok"))
        self.assertEqual(1, self.logged("login ghcr.io -u me --password-stdin$"))

    def test_docker_hub_logged_in_only_with_both_its_user_and_token(self) -> None:
        self.assertEqual(0, self.mirror(DOCKERHUB_USERNAME="ci-user"), self.out)
        self.assertEqual(0, self.logged("login"))
        self.assertEqual(0, self.mirror(DOCKERHUB_USERNAME="ci-user", DOCKERHUB_TOKEN="s3cret"), self.out)
        self.assertEqual((1, "s3cret"), (self.logged("login docker.io -u ci-user --password-stdin$"), self.read("login")))

    def test_a_pin_with_no_digest_fails(self) -> None:
        self.write("pins.tsv", "mongo\tdocker.io/library/mongo:8.3.17\tghcr.io/owner/mongo\n")
        self.assertEqual(1, self.mirror(), self.out)
        self.assertEqual(1, self.said(r"::error::mongo: the pin is not <repository>:<tag>@sha256:<digest> TAB <mirror>"))

    def test_a_pin_with_no_mirror_fails_and_the_next_row_is_still_mirrored(self) -> None:
        self.write("pins.tsv", f"redis\tdocker.io/library/redis:8@{DIGEST}\n"
                               f"mongo\tdocker.io/library/mongo:8.3.17@{DIGEST}\tghcr.io/owner/mongo\n")
        self.assertEqual(1, self.mirror(), self.out)
        self.assertEqual(DIGEST, self.ghcr_now())

    def test_a_table_pinning_nothing_fails(self) -> None:
        self.write("pins.tsv", "# only comments\n")
        self.assertEqual(1, self.mirror(), self.out)
        self.assertEqual(1, self.said(r"::error::.*pins no image"), self.out)


if __name__ == "__main__":
    unittest.main()
