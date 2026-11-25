package reachablelinkage.linkage

import reachablelinkage.classfile.ClassInfo
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.ClassId

/** Finds the method and line behind a [[Finding]] by re-reading just that class with debug info. Only the classes that actually have findings are ever re-read,
  * so the check itself stays cheap.
  */
final class Locator(index: ClassIndex):
  private val cache = new java.util.HashMap[Int, ClassInfo | Null]()

  private def reread(id: ClassId): ClassInfo | Null =
    if cache.containsKey(id.value) then cache.get(id.value)
    else
      val info = index.parseWithPositions(id)
      cache.put(id.value, info)
      info

  def locate(f: Finding): Location =
    if f.kind == RefKind.Hierarchy || f.kind == RefKind.Service then Location.Unknown
    else
      reread(f.conflict.inClass) match
        case null => Location.Unknown
        case info =>
          val pos = info.positions
          if pos == null then Location.Unknown
          else
            val table = f.kind match
              case RefKind.Method => pos.methodRefs
              case RefKind.Field  => pos.fieldRefs
              case _              => pos.classRefs
            val at = f.refIndex * 2
            if at + 1 >= table.length then Location.Unknown
            else
              val m = table(at)
              if m < 0 || m >= info.methods.length then Location.Unknown
              else
                val member = info.methods(m)
                Location(index.syms.name(member.name), index.syms.desc(member.desc), table(at + 1))
