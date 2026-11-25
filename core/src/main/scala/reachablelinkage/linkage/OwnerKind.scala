package reachablelinkage.linkage

/** What a method's owner is now, when that differs from what the call site was compiled against. */
enum OwnerKind:
  case Interface
  case Class
