package reachablelinkage.classfile

import reachablelinkage.symbols.{DescId, NameId}

/** A declared method or field member. */
final case class Member(name: NameId, desc: DescId, access: Int)
