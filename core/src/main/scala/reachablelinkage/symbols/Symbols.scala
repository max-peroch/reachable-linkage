package reachablelinkage.symbols

/** The three interning tables shared by one analysis. */
final class Symbols:
  private val classes = new Interner
  private val names   = new Interner
  private val descs   = new Interner

  def classId(internalName: String): ClassId = ClassId(classes.intern(internalName))
  def findClass(internalName: String): ClassId = ClassId(classes.lookup(internalName))
  def nameId(n: String): NameId              = NameId(names.intern(n))
  def descId(d: String): DescId              = DescId(descs.intern(d))

  /** How many names of each kind have been interned so far. */
  def classCount: Int = classes.size
  def nameCount: Int  = names.size
  def descCount: Int  = descs.size

  def className(c: ClassId): String = classes.string(c.value)
  def name(n: NameId): String       = names.string(n.value)
  def desc(d: DescId): String       = descs.string(d.value)
