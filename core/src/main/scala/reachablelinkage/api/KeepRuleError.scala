package reachablelinkage.api

import java.nio.file.Path

/** Why a keep rule, or a keep-rules file, was rejected. */
sealed abstract class KeepRuleError extends Product with Serializable {
  import KeepRuleError._
  def ordinal: Int
  def message: String = this match {
    case Unreadable(f, c) => s"cannot read $f: $c"
    case BadLine(f, n, p) => s"$f line $n: ${p.message}"
  }
}

object KeepRuleError {
  final case class Unreadable(file: Path, cause: String) extends KeepRuleError { def ordinal: Int = 0 }

  /** `line` is 1-based. */

  final case class BadLine(file: Path, line: Int, problem: KeepRuleError.Problem) extends KeepRuleError { def ordinal: Int = 1 }

  /** What is wrong with one rule, in a file or built directly. */
  sealed abstract class Problem extends Product with Serializable {
    import Problem._
    def ordinal: Int
    def message: String = this match {
      case UnknownDirective(w) =>
        s"unknown directive '$w' (expected keep-members, keep-no-arg-constructor, follow-class-values, keep-overrides, keep-bean-arguments, keep-built-arguments, keep-classes-named-in or keep-constructed-arguments)"
      case NoNames        => "no names"
      case InvalidName(n) => s"'$n' is not a class name"
    }
  }

  object Problem {
    final case class UnknownDirective(word: String) extends Problem { def ordinal: Int = 0 }
    case object NoNames                             extends Problem { def ordinal: Int = 1 }

    /** Not a class name such as `javax.inject.Inject` (dotted, with `$` for a nested class). */

    final case class InvalidName(name: String) extends Problem { def ordinal: Int = 2 }
  }
}
