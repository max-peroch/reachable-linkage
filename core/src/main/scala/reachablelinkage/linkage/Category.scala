package reachablelinkage.linkage

/** What kind of break a conflict is, as the facade names it. The names are those of `reachablelinkage.api.ConflictCategory` (which the Java facade needs as a
  * Java enum); `ConflictDescriptionSuite` keeps the two in step.
  */
private[reachablelinkage] sealed abstract class Category extends Product with Serializable {
  def ordinal: Int
}

private[reachablelinkage] object Category {
  case object CLASS_NOT_FOUND                 extends Category { def ordinal: Int = 0  }
  case object METHOD_SIGNATURE_NOT_FOUND      extends Category { def ordinal: Int = 1  }
  case object FIELD_NOT_FOUND                 extends Category { def ordinal: Int = 2  }
  case object UNREADABLE_CLASS                extends Category { def ordinal: Int = 3  }
  case object MISSING_PROVIDER                extends Category { def ordinal: Int = 4  }
  case object INACCESSIBLE_CLASS              extends Category { def ordinal: Int = 5  }
  case object INACCESSIBLE_MEMBER             extends Category { def ordinal: Int = 6  }
  case object ABSTRACT_METHOD_NOT_IMPLEMENTED extends Category { def ordinal: Int = 7  }
  case object INCOMPATIBLE_HIERARCHY          extends Category { def ordinal: Int = 8  }
  case object OVERRIDES_FINAL_METHOD          extends Category { def ordinal: Int = 9  }
  case object INTERFACE_CLASS_MISMATCH        extends Category { def ordinal: Int = 10 }
  case object ABSTRACT_INSTANTIATION          extends Category { def ordinal: Int = 11 }
  case object FINAL_FIELD_WRITE               extends Category { def ordinal: Int = 12 }
  case object MISSING_SIGNATURE_TYPE          extends Category { def ordinal: Int = 13 }
  case object NOT_EXPORTED                    extends Category { def ordinal: Int = 14 }
  val values: Array[Category] = Array(
    CLASS_NOT_FOUND,
    METHOD_SIGNATURE_NOT_FOUND,
    FIELD_NOT_FOUND,
    UNREADABLE_CLASS,
    MISSING_PROVIDER,
    INACCESSIBLE_CLASS,
    INACCESSIBLE_MEMBER,
    ABSTRACT_METHOD_NOT_IMPLEMENTED,
    INCOMPATIBLE_HIERARCHY,
    OVERRIDES_FINAL_METHOD,
    INTERFACE_CLASS_MISMATCH,
    ABSTRACT_INSTANTIATION,
    FINAL_FIELD_WRITE,
    MISSING_SIGNATURE_TYPE,
    NOT_EXPORTED
  )
  def fromOrdinal(ordinal: Int): Category = values(ordinal)
}
