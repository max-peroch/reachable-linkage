package reachablelinkage.classfile

/** How a call site invokes its target. The ordinal is the encoding stored in `ClassInfo.methodRefs`. */
private[reachablelinkage] enum CallKind:
  case Instance
  case Static
