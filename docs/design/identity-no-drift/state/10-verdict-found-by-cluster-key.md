# 10. Verdict found by cluster key

- **Where:** `verdictOf`, `corrected` (and its budget-spent fallback)
- **Verdict:** **(c) removed**

A verdict is now found by its listings' digest, under whichever key filed it (`heldFor`, `heldByDigest`). Specs: `AgreementCorrectionSpec` "stand as last decided after a deploy, whether or not it hands the stage its cluster keys", and harness property 3.
