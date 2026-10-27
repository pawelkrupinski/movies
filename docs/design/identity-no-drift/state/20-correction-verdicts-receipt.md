# 20. Correction verdicts' receipt

- **Where:** `AgreementCorrections`
- **Verdict:** **(b) fixed**

The receipt held family reads and asked ids, but not the TMDB records or candidates it reads. Every TMDB and venue read now goes into `StoredVerdict.reads` through the one seam (`Noting`, `receipting`), and a filing of one decides the correction again. Spec: `AgreementCorrectionSpec` "be decided again when a TMDB record it read is filed again".
