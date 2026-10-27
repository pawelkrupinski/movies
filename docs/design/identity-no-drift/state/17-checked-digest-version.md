# 17. `checked` (digest, version)

- **Where:** `AgreementStage`
- **Verdict:** (a) cache validity

Now the `Memo`'s check (603c81e77): one dirty index (checked at a version, no dependency filed since) for verdicts, corrections and candidates, proven by full validation every 12th apply (`validated_total{result="drifted"}`, zero). Its LateInputs run found the year read back as `Known(<year>)` against a recorded digest of its text (no verdict read again ever stood; fixed on main as 9627359ec).
