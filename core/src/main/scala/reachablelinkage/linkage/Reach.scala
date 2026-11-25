package reachablelinkage.linkage

/** How the call graph reaches something: by the application's own calls, through a reflection or keep rule, or only through deserialization. */
private[linkage] enum Reach:
  case Direct
  case Rule
  case Deserialization
