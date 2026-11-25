package reachablelinkage.api

import java.nio.file.Path

/** Why a keep rule, or a keep-rules file, was rejected. */
enum KeepRuleError:
  case Unreadable(file: Path, cause: String)

  /** `line` is 1-based. */
  case BadLine(file: Path, line: Int, problem: KeepRuleError.Problem)

  def message: String = this match
    case Unreadable(f, c) => s"cannot read $f: $c"
    case BadLine(f, n, p) => s"$f line $n: ${p.message}"

object KeepRuleError:
  /** What is wrong with one rule, in a file or built directly. */
  enum Problem:
    case UnknownDirective(word: String)
    case NoNames

    /** Not a class name such as `javax.inject.Inject` (dotted, with `$` for a nested class). */
    case InvalidName(name: String)

    def message: String = this match
      case UnknownDirective(w) =>
        s"unknown directive '$w' (expected keep-members, keep-no-arg-constructor, follow-class-values, keep-overrides, keep-bean-arguments, keep-built-arguments, keep-classes-named-in or keep-constructed-arguments)"
      case NoNames        => "no names"
      case InvalidName(n) => s"'$n' is not a class name"
