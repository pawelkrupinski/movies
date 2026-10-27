# 8. Held verdicts: the receipt was incomplete

- **Where:** `AgreementStage.held`, `identity_agreements`
- **Verdict:** **(b) fixed**

The receipt lists only family answers. A verdict now records everything it read, each answered or not: every family answer (unanswered ones too), the record of the film TMDB leans to, every TMDB lookup its fill makes (searches, records, release days, cast, details), the fill's production searches, and the year. The full check re-reads each one (`AgreementStage.read`). The feed now counts a filing of any TMDB record or search a held verdict read (`FamilyAnswerStore.readBy`, `AgreementStage.reads`). Specs: `AgreementStageSpec` "...the record of the film TMDB leans to is filed..." (the Dolly class), `AnswerFeedSpec` "count a TMDB record or search a kept verdict read".
