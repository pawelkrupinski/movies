package services.identity.vocabulary

/** How a rule's take may depend on the POPULATION, as [[RuleContract]] declares each rule's: (i) not at all, (ii) one
 *  way only, (iii) either way. */
enum RuleKind(val label: String) {
  case PopulationIndependent extends RuleKind("i")
  case Monotone extends RuleKind("ii")
  case NonMonotone extends RuleKind("iii")
}
