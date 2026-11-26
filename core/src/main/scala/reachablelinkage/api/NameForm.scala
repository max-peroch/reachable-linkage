package reachablelinkage.api

/** How the names of a rule are written. The facade writes them the way a user does (`a.b.Name`); the engine wants internal names (`a/b/Name`) for classes, and
  * the other forms as they are. One case per form, so what a kind's names look like, whether they are valid and how they cross over is said once.
  */
private[api] sealed abstract class NameForm extends Product with Serializable {
  import NameForm._
  def ordinal: Int

  /** Is `name` well formed in this form? */
  def isValid(name: String): Boolean = this match {
    case ClassNames  => JavaNames.isQualified(name)
    case CallNames   => JavaNames.isQualified(name) && NameForm.hasOwner(name)
    case FileNames   => name.nonEmpty && !name.startsWith("/") && !name.endsWith("/") && !name.exists(_.isWhitespace)
    case SuffixNames => name.nonEmpty && name.forall(Character.isJavaIdentifierPart)
  }

  /** The names as the engine wants them. */
  def toEngine(names: List[String]): Set[String] = this match {
    case ClassNames              => names.map(_.replace('.', '/')).toSet
    case CallNames               => names.map(NameForm.callToEngine).toSet
    case FileNames | SuffixNames => names.toSet
  }

  /** The names as the facade shows them, sorted so that a rule reads the same every time. */
  def toFacade(names: Set[String]): List[String] = this match {
    case ClassNames | CallNames  => names.toList.sorted.map(_.replace('/', '.'))
    case FileNames | SuffixNames => names.toList.sorted
  }
}

private[api] object NameForm {

  /** Class names: `a.b.Name` in the facade, `a/b/Name` in the engine. */

  case object ClassNames extends NameForm { def ordinal: Int = 0 }

  /** Calls, `owner.method`: the owner is a class name, the method follows the last dot. */

  case object CallNames extends NameForm { def ordinal: Int = 1 }

  /** File names or the end of a path (`reference.conf`, `META-INF/spring.factories`): kept as they are, since a dot is not a package separator. */

  case object FileNames extends NameForm { def ordinal: Int = 2 }

  /** Suffixes added to a class name (`_$logger`): kept as they are. */

  case object SuffixNames extends NameForm { def ordinal: Int = 3 }
  val values: Array[NameForm]             = Array(ClassNames, CallNames, FileNames, SuffixNames)
  def fromOrdinal(ordinal: Int): NameForm = values(ordinal)

  /** Does `owner.method` have an owner, that is, a dot that is not the first character? */
  private def hasOwner(call: String): Boolean = call.lastIndexOf('.') >= 1

  /** `a.b.Owner.method` as `a/b/Owner.method`: the owner is everything before the last dot, as an internal name. */
  private def callToEngine(call: String): String = {
    val dot = call.lastIndexOf('.')
    call.substring(0, dot).replace('.', '/') + call.substring(dot)
  }
}
