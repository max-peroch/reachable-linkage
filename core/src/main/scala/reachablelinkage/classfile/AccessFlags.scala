package reachablelinkage.classfile

/** The class-file access flags the checks read (JVMS 4.1, 4.5, 4.6), defined once. */
private[reachablelinkage] object AccessFlags {
  val Public: Int    = 0x0001
  val Private: Int   = 0x0002
  val Protected: Int = 0x0004
  val Static: Int    = 0x0008
  val Final: Int     = 0x0010
  val Interface: Int = 0x0200
  val Abstract: Int  = 0x0400

  implicit class FlagOps(private val flags: Int) extends AnyVal {

    /** Is the flag set? */
    @inline def has(flag: Int): Boolean = (flags & flag) != 0
  }
}
