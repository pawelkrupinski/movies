# 19. `candidateFilms` (digest, blindAt)

- **Where:** `AgreementCandidates`
- **Verdict:** **(b) fixed**

It cached TMDB searches by listing digest, so a re-asked search with a new candidate stayed unread. The cache now keeps its receipt and stands only while none of it was filed since (`standing`). Its reads also join the receipt being made. Spec: `AgreementPosterSpec` "take a candidate a search filed again finds".
