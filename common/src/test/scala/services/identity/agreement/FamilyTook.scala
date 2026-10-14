package services.identity.agreement

/** The verdict of a family that took `pick`, having weighed its record alone. */
object FamilyTook {
  def apply(pick: FamilyPick): FamilyVerdict = FamilyVerdict(pick.family, Some(pick), Seq(pick.record))
}
