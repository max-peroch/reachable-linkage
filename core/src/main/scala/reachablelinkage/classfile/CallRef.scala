package reachablelinkage.classfile

/** The fourth int of a `ClassInfo.methodRefs` entry: the [[CallKind]] in the low bit; a bit that says the reference is an `invokedynamic` method handle (a
  * lambda body or constructor reference) and not a call made at that point, since a handle's target runs later, somewhere else, so a `try` around the place
  * that made the handle does not protect it; and a bit that says the constant names an interface method (an `InterfaceMethodref`) and not a class method, which
  * the JVM checks against what the owner is when it links the call.
  */
private[reachablelinkage] object CallRef {
  private val HandleBit: Int    = 2
  private val InterfaceBit: Int = 4

  def direct(kind: CallKind, interface: Boolean): Int = kind.ordinal | (if (interface) InterfaceBit else 0)
  def handle(kind: CallKind, interface: Boolean): Int = direct(kind, interface) | HandleBit

  def kind(code: Int): CallKind       = CallKind.fromOrdinal(code & 1)
  def isHandle(code: Int): Boolean    = (code & HandleBit) != 0
  def isInterface(code: Int): Boolean = (code & InterfaceBit) != 0
}
