package reachablelinkage.classfile

private[reachablelinkage] object Keys:
  /** One long for a (name, descriptor) pair: the name id in the high half, the descriptor id in the low. */
  inline def of(name: Int, desc: Int): Long = (name.toLong << 32) | (desc.toLong & 0xffffffffL)
