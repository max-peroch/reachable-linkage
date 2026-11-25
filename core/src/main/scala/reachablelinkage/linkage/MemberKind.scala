package reachablelinkage.linkage

/** What a lookup is for. Static and instance methods resolve differently through interfaces. */
private[linkage] enum MemberKind:
  case InstanceMethod
  case StaticMethod
  case Field
