# 9. Year read from the clock

- **Where:** `AgreementStage.resolved`
- **Verdict:** **(b) fixed**

The year is in the receipt, and a year turning clears `checked` and the last-apply shortcut. Spec: "decide a kept verdict again when the year turns". Cost: once a year, every verdict is read again; the ones whose year read moved are decided again.
