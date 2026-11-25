package reachablelinkage.api

/**
 * Readable names for what the Scala compilers generate, for reports only (the conflicts keep the raw names).
 * `Foo$` is `object Foo`, `$anonfun$bar$1` an anonymous function in `bar`, `foo$extension` an extension method of a
 * value class, `$plus` is `+`. Java's `lambda$run$0` gets the same treatment. A name that matches nothing is
 * returned as it is, so Java code is untouched.
 */
private[api] object ScalaNames:
  private val operators: List[(String, String)] = List(
    "$plus" -> "+", "$minus" -> "-", "$times" -> "*", "$div" -> "/", "$eq" -> "=", "$less" -> "<", "$greater" -> ">",
    "$bang" -> "!", "$hash" -> "#", "$percent" -> "%", "$up" -> "^", "$amp" -> "&", "$bar" -> "|", "$tilde" -> "~",
    "$qmark" -> "?", "$colon" -> ":", "$bslash" -> "\\", "$at" -> "@")

  private val AnonFun   = """^\$anonfun\$(.*?)(?:\$\d+)+(?:\$adapted)?$""".r
  // Scala 3 names a lambda after its method: `bar$$anonfun$1`.
  private val AnonFun3  = """^(.+?)\$\$anonfun\$\d+(?:\$adapted)?$""".r
  private val Lambda    = """^lambda\$(.*?)\$\d+$""".r
  private val Extension = """^(.+)\$extension\d*$""".r
  private val Default   = """^(.+)\$default\$(\d+)$""".r

  /** A source-like name for a method. */
  def method(name: String): String = name match
    case "<init>"          => "constructor"
    case "<clinit>"        => "static initializer"
    case "$init$"          => "trait initializer"
    case AnonFun(inner)    => s"anonymous function in ${if inner.isEmpty then "<unknown>" else method(inner)}"
    case AnonFun3(inner)   => s"anonymous function in ${method(inner)}"
    case Lambda(inner)     => s"lambda in $inner"
    case Extension(inner)  => s"${method(inner)} (extension method)"
    case Default(inner, n) => s"default argument $n of ${method(inner.replace("$lessinit$greater", "<init>"))}"
    case other             => operators.foldLeft(other)((acc, op) => acc.replace(op._1, op._2))

  private val AnonClass    = """^(.*?)(?:\$+anon\$\d+)+$""".r
  private val AnonFunClass = """^(.*?)\$+anonfun\$.*$""".r

  /** A source-like name for a class: the package is kept, the simple name is translated. */
  def clazz(name: String): String =
    val dot           = name.lastIndexOf('.')
    val (pkg, simple) = if dot < 0 then ("", name) else (name.substring(0, dot + 1), name.substring(dot + 1))
    simple match
      case AnonClass(outer) if outer.nonEmpty    => s"anonymous class in ${clazz(pkg + outer)}"
      case AnonFunClass(outer) if outer.nonEmpty => s"anonymous function in ${clazz(pkg + outer)}"
      case s if s.endsWith("$") && s.length > 1  => s"object $pkg${s.stripSuffix("$").replace('$', '.')}"
      case s                                     => pkg + s

  /** Does the target member name say the reference comes from a Scala 3 `inline` expansion? */
  def isInlineAccessor(member: String): Boolean = member.startsWith("inline$") || member.contains("$inline$")
