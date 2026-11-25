package reachablelinkage.linkage

/** What kind of break a conflict is, as the facade names it. The names are those of `reachablelinkage.api.ConflictCategory` (which the Java facade needs as a
  * Java enum); `ConflictDescriptionSuite` keeps the two in step.
  */
private[reachablelinkage] enum Category:
  case CLASS_NOT_FOUND, METHOD_SIGNATURE_NOT_FOUND, FIELD_NOT_FOUND, UNREADABLE_CLASS, MISSING_PROVIDER, INACCESSIBLE_CLASS,
    INACCESSIBLE_MEMBER, ABSTRACT_METHOD_NOT_IMPLEMENTED, INCOMPATIBLE_HIERARCHY, OVERRIDES_FINAL_METHOD,
    INTERFACE_CLASS_MISMATCH, ABSTRACT_INSTANTIATION, FINAL_FIELD_WRITE, MISSING_SIGNATURE_TYPE, NOT_EXPORTED
