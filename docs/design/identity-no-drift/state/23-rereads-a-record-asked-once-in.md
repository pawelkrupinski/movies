# 23. `rereads` (a record asked once in the stage's life) plus `waitsOn`

- **Where:** `AgreementCandidates`
- **Verdict:** **(c) fixed, deleted**

Whether a year-only record still waits depended on whether THIS stage instance asked. It now depends on the question memory (the read was attempted; the asker marks an answer as one), and it is in the fill's receipt (`tried|...`). An attempt marked since the last apply reads every receipt again. Spec: `AgreementBroadcastSpec` "...hold no take, in a stage that never asked it too".
