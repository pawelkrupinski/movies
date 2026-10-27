# 8a. What a take reads afresh each apply (the poster vote's records, the broadcast join's, synopses, IMDb finds)

- **Where:** `AgreementTakes`, the last-apply shortcut
- **Verdict:** **(b) fixed**

These are not stored, but the quiet-apply shortcut handed back the last result until the version moved. Every TMDB read goes through `Noting`, and the last full apply's records, searches and finds are what `AgreementStage.reads` tells the feed (`FamilyAnswerStore.readBy`, finds included).
