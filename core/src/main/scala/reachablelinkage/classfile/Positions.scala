package reachablelinkage.classfile

/** Where each reference sits in the class file, collected only when a class is re-read for reporting. One entry per reference, in the same order as the
  * matching `ClassInfo` array; each entry is two ints: the index into `ClassInfo.methods` of the enclosing method, and its source line (0 if unknown).
  */
private[reachablelinkage] final class Positions(
    val methodRefs: Array[Int],
    val fieldRefs: Array[Int],
    val classRefs: Array[Int]
)
